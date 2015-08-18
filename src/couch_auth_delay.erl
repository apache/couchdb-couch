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

%% This module monitors authentication failures and calculates
%% how long an authentication request should be delayed by.

%% The current lockout scheme is that after X (default is 5) incorrect
%% passwords all requests from that source ip address incur the full
%% delay penalty (default is 30 seconds).

-module(couch_auth_delay).
-behaviour(gen_server).
-vsn(1).

-record(entry, {key, last_updated, failures, locked_until}).
-record(state, {tab}).

% public API

-export([success/2, failure/2, get_delay_secs/2, clear/0]).

% gen_server API
-export([start_link/0, init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).


%% Report a password success for user
success(User, PeerAddress) when is_list(User), is_list(PeerAddress) ->
    gen_server:cast(?MODULE, {success, User, PeerAddress}).


%% Report a password failure for user
failure(User, PeerAddress) when is_list(User), is_list(PeerAddress) ->
    gen_server:cast(?MODULE, {fail, User, PeerAddress}).


get_delay_secs(User, PeerAddress) when is_list(User), is_list(PeerAddress) ->
    Now = timestamp_secs(),
    case ets:lookup(?MODULE, key(User, PeerAddress)) of
        [#entry{locked_until = LockedUntil}]
          when is_integer(LockedUntil), LockedUntil > Now ->
            LockedUntil - Now;
        _ ->
            0
    end.


clear() ->
    gen_server:cast(?MODULE, clear).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    Tab = ets:new(?MODULE, [set, protected, named_table,
                                {keypos, #entry.key}]),
    schedule_next_stale_check(),
    {ok, #state{tab = Tab}}.


handle_call(_Msg, _From, State) ->
    {reply, {error, unexpected_msg}, State}.


handle_cast({success, User, PeerAddress}, State) ->
    couch_log:notice("User ~s from ~s authenticate successfully, removing "
                     "delay", [User, PeerAddress]),
    true = ets:delete(State#state.tab, key(User, PeerAddress)),
    {noreply, State};

handle_cast({fail, User, PeerAddress}, State) ->
    case ets:lookup(State#state.tab, key(User, PeerAddress)) of
        [] ->
            ets:insert_new(State#state.tab,
                           [new_entry(User, PeerAddress)]);
        [#entry{} = Entry0] ->
            Entry1 = bump_entry(Entry0),
            case is_locked(Entry1) of
                true ->
                    couch_log:warning(
                      "User ~s from ~s locked out due to "
                      "too many password failures",
                      [User, PeerAddress]);
                false ->
                    ok
            end,
            ets:insert(State#state.tab, [Entry1])
    end,
    {noreply, State};

handle_cast(clear, State) ->
    ets:delete_all_objects(State#state.tab),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(clear_stale_entries, State) ->
    schedule_next_stale_check(),

    Stale = timestamp_secs() - lockout_secs(),
    MatchSpec = [{#entry{last_updated = '$1', _ = '_'},
                  [], [{'<', '$1', Stale}]}],
    case ets:select_delete(State#state.tab, MatchSpec) of
        0 ->
            ok;
        NumDeleted ->
            couch_log:notice("Removed ~p stale lockout entries",
                             [NumDeleted])
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%% private functions
new_entry(User, PeerAddress) ->
    #entry{key = key(User, PeerAddress),
           last_updated = timestamp_secs(),
           failures = 1}.


bump_entry(#entry{} = Entry0) ->
    Entry1 = Entry0#entry{failures = Entry0#entry.failures + 1,
                          last_updated = timestamp_secs()},
    maybe_lockout(Entry1, max_attempts()).


maybe_lockout(#entry{failures = MaxAttempts} = Entry, MaxAttempts) ->
    Entry#entry{locked_until = timestamp_secs() + lockout_secs()};
maybe_lockout(#entry{} = Entry, _MaxAttempts) ->
    Entry.


is_locked(#entry{locked_until = undefined}) ->
    false;
is_locked(#entry{}) ->
    true.

timestamp_secs() ->
    %% use erlang:monotonic_time() in 18
    {NowMS, NowS, _} = os:timestamp(),
    NowMS * 1000000 + NowS.


key(User, PeerAddress) ->
    {User, PeerAddress}.


schedule_next_stale_check() ->
    erlang:send_after(timeout_interval_millis(), self(),
                      clear_stale_entries).

%% The duration of a lockout in seconds
lockout_secs() ->
    config:get_integer("couch_auth_lockout", "lockout_secs", 30).


%% The maximum number of failed attempts before lockout
max_attempts() ->
    config:get_integer("couch_auth_lockout", "max_attempts", 5).



%% how often to timeout and check for stale entries
timeout_interval_millis() ->
    lockout_secs() * 1000.

