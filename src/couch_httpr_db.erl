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

-module(couch_httpr_db).


-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2
]).


-include_lib("webmachine/include/webmachine.hrl").
-include_lib("chttpd2/include/chttpd2.hrl").


-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.


content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.


content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.


allowed_methods(ReqData, Context) ->
    {['GET', 'PUT'], ReqData, Context}.


-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    DbId = wrq:path_info(db_id, ReqData),
    {ok, DbInfo} = fabric:get_db_info(DbId),
    {?JSON_ENCODE({DbInfo}), ReqData, State}.


-spec from_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
from_json(ReqData, State) ->
    DbId = wrq:path_info(db_id, ReqData),
    %% todo: parse options
    %% Options = chttpd2_util:json_body(ReqData),
    %% todo: handle pattern mismatch
    %% ok = fabric:create_db(DbId, Options),
    %% {true, ReqData, State}.
    {{halt, 202}, ReqData, State}.
