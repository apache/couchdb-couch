% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_proc_manager).
-behaviour(gen_server).
-behaviour(config_listener).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/0, get_proc_count/0, new_proc/1]).

% config_listener api
-export([handle_config_change/5]).

-include_lib("couch/include/couch_db.hrl").

-record(state, {
    tab,
    config,
    proc_counts,
    waiting
}).

-record(client, {
    timestamp,
    from,
    lang,
    ddoc,
    ddoc_key
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_proc_count() ->
    gen_server:call(?MODULE, get_proc_count).

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, #state{
        tab = ets:new(procs, [ordered_set, {keypos, #proc.pid}]),
        config = get_proc_config(),
        proc_counts = dict:new(),
        waiting = ets:new(couch_proc_manage_waiting,
                [ordered_set, {keypos, #client.timestamp}])
    }}.

handle_call(get_table, _From, State) ->
    {reply, State#state.tab, State};

handle_call(get_proc_count, _From, State) ->
    {reply, ets:info(State#state.tab, size), State};

handle_call({get_proc, #doc{body={Props}}=DDoc, DDocKey}, From, State) ->
    {ClientPid, _} = From,
    Lang = couch_util:to_binary(
            couch_util:get_value(<<"language">>, Props, <<"javascript">>)),
    IterFun = fun(Proc, Acc) ->
        case lists:member(DDocKey, Proc#proc.ddoc_keys) of
            true ->
                {stop, assign_proc(State#state.tab, ClientPid, Proc)};
            false ->
                {ok, Acc}
        end
    end,
    TeachFun = fun(Proc0, Acc) ->
        try
            {ok, Proc1} = teach_ddoc(DDoc, DDocKey, Proc0),
            {stop, assign_proc(State#state.tab, ClientPid, Proc1)}
        catch _:_ ->
            {ok, Acc}
        end
    end,
    Client = #client{from=From, lang=Lang, ddoc=DDoc, ddoc_key=DDocKey},
    find_proc(State, Client, [IterFun, TeachFun]);

handle_call({get_proc, Lang}, From, State) ->
    {ClientPid, _} = From,
    IterFun = fun(Proc, _Acc) ->
        {stop, assign_proc(State#state.tab, ClientPid, Proc)}
    end,
    Client = #client{from=From, lang=couch_util:to_binary(Lang)},
    find_proc(State, Client, [IterFun]);

handle_call({ret_proc, #proc{client=Ref, lang=Lang0} = Proc}, _From, State) ->
    erlang:demonitor(Ref, [flush]),
    Lang = couch_util:to_binary(Lang0),
    % We need to check if the process is alive here, as the client could be
    % handing us a #proc{} with a dead one.  We would have already removed the
    % #proc{} from our own table, so the alternative is to do a lookup in the
    % table before the insert.  Don't know which approach is cheaper.
    {reply, true, return_proc(State, Proc#proc{lang=Lang})};

handle_call(_Call, _From, State) ->
    {reply, ignored, State}.

handle_cast({os_proc_idle, Pid}, #state{tab=Tab, proc_counts=Counts}=State0) ->
    Limit = list_to_integer(
            config:get("query_server_config", "os_process_soft_limit", "100")),
    State = case ets:lookup(Tab, Pid) of
        [#proc{client=nil, lang=Lang}] ->
            case dict:find(Lang, Counts) of
                {ok, Count} when Count > Limit ->
                    ?LOG_INFO("Closing idle OS Process: ~p", [Pid]),
                    ets:delete(Tab, Pid),
                    case is_process_alive(Pid) of
                        true ->
                            unlink(Pid),
                            gen_server:cast(Pid, stop);
                        _ ->
                            ok
                    end,
                    State0#state{
                        proc_counts=dict:update_counter(Lang, -1, Counts)
                    };
                {ok, _} ->
                    State0
            end;
        _ ->
            State0
    end,
    {noreply, State};
handle_cast(reload_config, State) ->
    {noreply, State#state{config = get_proc_config()}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(shutdown, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', _, {ok, Proc0, {ClientPid,_} = From}}, State) ->
    link(Proc0#proc.pid),
    Proc = assign_proc(State#state.tab, ClientPid, Proc0),
    gen_server:reply(From, {ok, Proc, State#state.config}),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    #state{proc_counts=Counts, waiting=Waiting} = State,
    ?LOG_INFO("~p ~p died ~p", [?MODULE, Pid, Reason]),
    MaybeProc = ets:lookup(State#state.tab, Pid),
    ets:delete(State#state.tab, Pid),
    case MaybeProc of
        [#proc{lang=Lang}] ->
            case get_waiting_client(Waiting, Lang) of
                nil ->
                    {noreply, State#state{
                        proc_counts=dict:update_counter(Lang, -1, Counts)
                    }};
                Client ->
                    spawn_link(?MODULE, new_proc, [Client]),
                    {noreply, State}
            end;
        [] ->
            {noreply, State}
    end;

handle_info({'DOWN', Ref, _, _, _Reason}, State0) ->
    case ets:match_object(State0#state.tab, #proc{client=Ref, _='_'}) of
    [] ->
        {noreply, State0};
    [#proc{} = Proc] ->
        {noreply, return_proc(State0, Proc)}
    end;

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};

handle_info(restart_config_lister, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{tab=Tab}) ->
    ets:foldl(fun(#proc{pid=P}, _) -> couch_util:shutdown_sync(P) end, 0, Tab),
    ok.

code_change(_OldVsn, #state{tab = Tab} = State, _Extra) ->
    NewTab = ets:new(procs, [ordered_set, {keypos, #proc.pid}]),
    true = ets:insert(NewTab, ets:tab2list(Tab)),
    true = ets:delete(Tab),
    {ok, State#state{tab = NewTab}}.

handle_config_change("query_server_config", _, _, _, _) ->
    gen_server:cast(?MODULE, reload_config),
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

find_proc(State, Client, [Fun|FindFuns]) ->
    try iter_procs(State#state.tab, Client#client.lang, Fun, nil) of
    {not_found, _} ->
        find_proc(State, Client, FindFuns);
    {ok, Proc} ->
        {reply, {ok, Proc, State#state.config}, State}
    catch error:Reason ->
        ?LOG_ERROR("~p ~p ~p", [?MODULE, Reason, erlang:get_stacktrace()]),
        {reply, {error, Reason}, State}
    end;
find_proc(State, Client, []) ->
    {noreply, maybe_spawn_proc(State, Client)}.

iter_procs(Tab, Lang, Fun, Acc) when is_list(Lang) ->
    iter_procs(Tab, list_to_binary(Lang), Fun, Acc);
iter_procs(Tab, Lang, Fun, Acc) ->
    Pattern = #proc{lang=Lang, client=nil, _='_'},
    MSpec = [{Pattern, [], ['$_']}],
    case ets:select_reverse(Tab, MSpec, 25) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation ->
            iter_procs(Continuation, Fun, Acc)
    end.

iter_procs({[], Continuation0}, Fun, Acc) ->
    case ets:select_reverse(Continuation0) of
        '$end_of_table' ->
            {not_found, Acc};
        Continuation1 ->
            iter_procs(Continuation1, Fun, Acc)
    end;
iter_procs({[Proc | Rest], Continuation}, Fun, Acc0) ->
    case Fun(Proc, Acc0) of
        {ok, Acc1} ->
            iter_procs({Rest, Continuation}, Fun, Acc1);
        {stop, Acc1} ->
            {ok, Acc1}
    end.

new_proc(#client{ddoc=undefined, ddoc_key=undefined}=Client) ->
    #client{from=From, lang=Lang} = Client,
    case new_proc_int(From, Lang) of
    {ok, Proc} ->
        exit({ok, Proc, From});
    Error ->
        gen_server:reply(From, {error, Error})
    end;

new_proc(Client) ->
    #client{from=From, lang=Lang, ddoc=DDoc, ddoc_key=DDocKey} = Client,
    case new_proc_int(From, Lang) of
    {ok, NewProc} ->
        case proc_with_ddoc(DDoc, DDocKey, [NewProc]) of
        {ok, Proc} ->
            exit({ok, Proc, From});
        {error, Reason} ->
            gen_server:reply(From, {error, Reason})
        end;
    Error ->
        gen_server:reply(From, {error, Error})
    end.

new_proc_int(From, Lang) when is_binary(Lang) ->
    new_proc_int(From, binary_to_list(Lang));
new_proc_int(From, Lang) when is_list(Lang) ->
    case config:get("query_servers", Lang) of
    undefined ->
        case config:get("native_query_servers", Lang) of
        undefined ->
            gen_server:reply(From, {unknown_query_language, Lang});
        SpecStr ->
            {ok, {M,F,A}} = couch_util:parse_term(SpecStr),
            {ok, Pid} = apply(M, F, A),
            make_proc(Pid, Lang, M)
        end;
    Command ->
        {ok, Pid} = couch_os_process:start_link(Command),
        make_proc(Pid, Lang, couch_os_process)
    end.

proc_with_ddoc(DDoc, DDocKey, Procs) ->
    Filter = fun(#proc{ddoc_keys=Keys}) -> not lists:member(DDocKey, Keys) end,
    case lists:dropwhile(Filter, Procs) of
    [DDocProc|_] ->
        {ok, DDocProc};
    [] ->
        teach_any_proc(DDoc, DDocKey, Procs)
    end.

teach_any_proc(DDoc, DDocKey, [Proc|Rest]) ->
    try
        teach_ddoc(DDoc, DDocKey, Proc)
    catch _:_ ->
        teach_any_proc(DDoc, DDocKey, Rest)
    end;
teach_any_proc(_, _, []) ->
    {error, noproc}.

teach_ddoc(DDoc, {DDocId, _Rev}=DDocKey, #proc{ddoc_keys=Keys}=Proc) ->
    % send ddoc over the wire
    % we only share the rev with the client we know to update code
    % but it only keeps the latest copy, per each ddoc, around.
    true = couch_query_servers:proc_prompt(Proc, [<<"ddoc">>, <<"new">>,
        DDocId, couch_doc:to_json_obj(DDoc, [])]),
    % we should remove any other ddocs keys for this docid
    % because the query server overwrites without the rev
    Keys2 = [{D,R} || {D,R} <- Keys, D /= DDocId],
    % add ddoc to the proc
    {ok, Proc#proc{ddoc_keys=[DDocKey|Keys2]}}.

make_proc(Pid, Lang, Mod) ->
    Proc = #proc{
        lang = Lang,
        pid = Pid,
        prompt_fun = {Mod, prompt},
        prompt_many_fun = {Mod, prompt_many},
        set_timeout_fun = {Mod, set_timeout},
        stop_fun = {Mod, stop}
    },
    unlink(Pid),
    {ok, Proc}.

assign_proc(Tab, ClientPid, #proc{client=nil}=Proc0) when is_pid(ClientPid) ->
    Proc = Proc0#proc{client = erlang:monitor(process, ClientPid)},
    ets:insert(Tab, Proc),
    Proc;
assign_proc(Tab, #client{}=Client, #proc{client=nil}=Proc) ->
    {Pid, _} = Client#client.from,
    assign_proc(Tab, Pid, Proc).

return_proc(State, #proc{pid=Pid, lang=Lang} = Proc) ->
    #state{tab=Tab, waiting=Waiting} = State,
    case is_process_alive(Pid) of true ->
        case get_waiting_client(Waiting, Lang) of
            nil ->
                gen_server:cast(Pid, garbage_collect),
                ets:insert(Tab, Proc#proc{client=nil}),
                State;
            #client{}=Client ->
                From = Client#client.from,
                assign_proc(Tab, Client, Proc#proc{client=nil}),
                gen_server:reply(From, {ok, Proc, State#state.config}),
                State
        end;
    false ->
        ets:delete(Tab, Pid),
        case get_waiting_client(Waiting, Lang) of
            nil ->
                State;
            #client{}=Client ->
                maybe_spawn_proc(State, Client)
        end
    end.

maybe_spawn_proc(State, Client) ->
    #state{proc_counts=Counts, waiting=Waiting} = State,
    #client{lang=Lang} = Client,
    Limit = list_to_integer(config:get(
                "query_server_config", "os_process_limit", "100")),
    case dict:find(Lang, Counts) of
    {ok, Limit} ->
        add_waiting_client(Waiting, Client),
        State;
    _ ->
        spawn_link(?MODULE, new_proc, [Client]),
        State#state{
            proc_counts=dict:update_counter(Lang, 1, Counts)
        }
    end.

add_waiting_client(Tab, Client) ->
    ets:insert(Tab, Client#client{timestamp=now()}).

get_waiting_client(Tab, Lang) when is_list(Lang) ->
    get_waiting_client(Tab, couch_util:to_binary(Lang));
get_waiting_client(Tab, Lang) ->
    case ets:match_object(Tab, #client{lang=Lang, _='_'}, 1) of
        '$end_of_table' ->
            nil;
        {[#client{}=Client], _} ->
            ets:delete(Tab, Client#client.timestamp),
            Client
    end.

get_proc_config() ->
    Limit = config:get("query_server_config", "reduce_limit", "true"),
    Timeout = config:get("couchdb", "os_process_timeout", "5000"),
    {[
        {<<"reduce_limit">>, list_to_atom(Limit)},
        {<<"timeout">>, list_to_integer(Timeout)}
    ]}.
