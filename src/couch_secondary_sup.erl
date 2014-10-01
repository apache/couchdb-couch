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

-module(couch_secondary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local,couch_secondary_services}, ?MODULE, []).

init([]) ->
    SecondarySupervisors = [
        {couch_plugin_event,
            {gen_event, start_link, [{local, couch_plugin}]},
            permanent,
            brutal_kill,
            worker,
            dynamic}
    ],
    Children0 = SecondarySupervisors ++ [
        begin
            {ok, {Module, Fun, Args}} = couch_util:parse_term(SpecStr),

            {list_to_atom(Name),
                {Module, Fun, Args},
                permanent,
                brutal_kill,
                worker,
                [Module]}
        end
        || {Name, SpecStr}
        <- config:get("daemons"), SpecStr /= ""],
    Children = [{
        hash_fun_lru,
        {ets_lru, start_link, [hash_fun_lru, lru_opts()]},
        permanent,
        5000,
        worker,
        [ets_lru]
    }|Children0],
    {ok, {{one_for_one, 50, 3600}, Children}}.

lru_opts() ->
    case application:get_env(couch, hash_fun_lru_max_objects) of
        {ok, MxObjs} when is_integer(MxObjs), MxObjs > 0 ->
            [{max_objects, MxObjs}];
        _ ->
            []
    end ++
    case application:get_env(couch, hash_fun_lru_max_size) of
        {ok, MxSize} when is_integer(MxSize), MxSize > 0 ->
            [{max_size, MxSize}];
        _ ->
            []
    end ++
    case application:get_env(couch, hash_fun_lru_max_lifetime) of
        {ok, MxLT} when is_integer(MxLT), MxLT > 0 ->
            [{max_lifetime, MxLT}];
        _ ->
            []
    end.
