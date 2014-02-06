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

-module(couch_app).

-behaviour(application).

-include("couch_db.hrl").

-export([start/2, stop/1]).

-define(CONF_FILES, ["couch.ini", "local.ini"]).

start(_Type, _Args) ->
    couch_util:start_app_deps(couch),
    IniFiles = get_ini_files(),
    couch_server_sup:start_link(IniFiles).

stop(_) ->
    ok.

get_ini_files() ->
    DefaultConfDir =  filename:join([code:root_dir(), "./etc"]),
    Defaults = lists:map(fun(FName) ->
                    filename:join(DefaultConfDir, FName)
            end, ?CONF_FILES),
    io:format("default files ~p~n", [couch:get_app_env(config_files,
                                                       Defaults)]),
    couch:get_app_env(config_files, Defaults).
