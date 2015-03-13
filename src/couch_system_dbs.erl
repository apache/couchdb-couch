-module(couch_system_dbs).

-behaviour(config_listener).
-vsn(1).

-export([subscribe/0, before_doc_update/1, after_doc_update/1]).
-export([system_dbs/0, local_dbs/0, options/1, is_system_db/1]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

subscribe() ->
    ok = config:listen_for_changes(?MODULE, nil),
    register_all().

before_doc_update(Name) ->
    Callbacks = get_env(system_dbs_before_doc_update, []),
    proplists:get_value(Name, Callbacks, undefined).

after_doc_update(Name) ->
    Callbacks = get_env(system_dbs_after_doc_update, []),
    proplists:get_value(Name, Callbacks, undefined).

system_dbs() ->
    get_env(system_dbs_all, []).

local_dbs() ->
    get_env(system_dbs_local, []).

options(Name) ->
    Options = get_env(system_dbs_options, []),
    proplists:get_value(Name, Options, []).

is_system_db(Name) ->
    lists:keymember(Name, 1, get_env(system_dbs_all, [])).

handle_config_change("couchdb", "db_definitions", _, _, _) ->
    register_all(),
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) -> ok;
handle_config_terminate(_, _, _) ->
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, nil)
    end).

%% private functions

register_all() ->
    Definitions = definitions(),
    register_system_dbs(Definitions),
    register_local_dbs(Definitions),
    register_sys_callbacks(Definitions),
    register_options(Definitions).

definitions() ->
    ConfigStr = config:get("couchdb", "db_definitions", "[]"),
    {ok, Modules} = couch_util:parse_term(ConfigStr),
    lists:flatten([define(Module) || Module <- Modules]).

get_env(Key, Default) ->
    case application:get_env(couch, Key) of
    undefined ->
        Default;
    {ok, Value} ->
        Value
    end.

define(Module) ->
    [{Id, Module} || Id <- Module:databases()].

register_system_dbs(Definitions) ->
    application:set_env(couch, system_dbs_all, Definitions).

register_local_dbs(Definitions) ->
    application:set_env(couch, system_dbs_local, local_dbs(Definitions)).

register_sys_callbacks(Definitions) ->
    {Before, After} = callbacks(Definitions),
    application:set_env(couch, system_dbs_before_doc_update, Before),
    application:set_env(couch, system_dbs_after_doc_update, After).

register_options(Definitions) ->
    application:set_env(couch, system_dbs_options, options_int(Definitions)).

local_dbs(Definitions) ->
    lists:filtermap(fun({Name, Module}) ->
        case lists:member(local, Module:options(Name)) of
        true ->
            {true, list_to_binary(Name)};
        false ->
            false
        end
    end, Definitions).

callbacks(Definitions) ->
    lists:foldl(fun({Id, Options}, {BeforeAcc0, AfterAcc0}) ->
        BeforeAcc =
            append_if_callback_set(before_doc_update, Id, Options, BeforeAcc0),
        AfterAcc =
            append_if_callback_set(before_doc_update, Id, Options, AfterAcc0),
        {BeforeAcc, AfterAcc}
    end, {[], []}, options_int(Definitions)).

options_int(Definitions) ->
    [{Id, Module:options(Id)} || {Id, Module} <- Definitions].

append_if_callback_set(Type, Id, Options, Acc) ->
    case proplists:get_value(Type, Options) of
    undefined ->
        Acc;
    Fun ->
        [{Id, Fun}|Acc]
    end.
