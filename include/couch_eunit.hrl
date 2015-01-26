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

-include_lib("eunit/include/eunit.hrl").

-define(BUILDDIR,
    fun() ->
        case os:getenv("BUILDDIR") of
            false ->
                throw("BUILDDIR environment variable must be set");
            Dir ->
                Dir
        end
    end).
-define(CONFIG_DEFAULT,
    filename:join([?BUILDDIR(), "tmp", "etc", "default_eunit.ini"])).
-define(CONFIG_CHAIN, [
    ?CONFIG_DEFAULT,
    filename:join([?BUILDDIR(), "tmp", "etc", "local_eunit.ini"]),
    filename:join([?BUILDDIR(), "tmp", "etc", "eunit.ini"])]).
-define(FIXTURESDIR,
    filename:join([?BUILDDIR(), "src", "couch", "test", "fixtures"])).
-define(TEMPDIR,
    filename:join([?BUILDDIR(), "tmp", "tmp_data"])).

-define(tempfile,
    fun() ->
        {A, B, C} = erlang:now(),
        N = node(),
        FileName = lists:flatten(io_lib:format("~p-~p.~p.~p", [N, A, B, C])),
        filename:join([?TEMPDIR, FileName])
    end).
-define(tempdb,
    fun() ->
            Nums = tuple_to_list(erlang:now()),
            Prefix = "eunit-test-db",
            Suffix = lists:concat([integer_to_list(Num) || Num <- Nums]),
            list_to_binary(Prefix ++ "-" ++ Suffix)
    end).
-define(docid,
    fun() ->
        {A, B, C} = erlang:now(),
        lists:flatten(io_lib:format("~p~p~p", [A, B, C]))
    end).

%% Borrowed from https://github.com/richcarl/eunit/blob/master/include/eunit.hrl#L200-L219
%% TODO: get rid of this once R14* is no longer supported
-ifndef(assertNotMatch).
-define(assertNotMatch(Guard, Expr),
    begin
        ((fun () ->
            __V = (Expr),
            case __V of
                Guard -> erlang:error({assertNotMatch_failed,
                    [{module, ?MODULE},
                    {line, ?LINE},
                    {expression, (??Expr)},
                    {pattern, (??Guard)},
                    {value, __V}]});
                _ -> ok
            end
        end)())
    end).
-endif.
