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

-module(couch).

-export([get_app_env/2,
         version/0,
         release_version/0,
         start/0,
         stop/0,
         restart/0,
         reload/0]).

get_app_env(Env, Default) ->
    case application:get_env(couch, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

version() ->
    case application:get_key(couch, vsn) of
        {ok, FullVersion} ->
            hd(string:tokens(FullVersion, "-"));
        _ ->
            "0.0.0"
    end.

release_version() ->
    case application:get_env(couch, couch_rel) of
        {ok, Vsn} ->
            Vsn;
        _ ->
            "0.0.0"
    end.

start() ->
    couch_util:start_app_deps(couch),
    application:start(couch).

stop() ->
    application:stop(couch).

restart() ->
    case stop() of
    ok ->
        start();
    {error, {not_started,couch}} ->
        start();
    {error, Reason} ->
        {error, Reason}
    end.

reload() ->
    case supervisor:terminate_child(couch_server_sup, couch_config) of
    ok ->
        supervisor:restart_child(couch_server_sup, couch_config);
    {error, Reason} ->
        {error, Reason}
    end.
