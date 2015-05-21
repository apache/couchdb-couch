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

-module(couchdb_mrview_etag_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(DDOC, {[
    {<<"_id">>, <<"_design/foo">>},
    {<<"language">>, <<"javascript">>},
    {<<"views">>, {[
        {<<"bar">>, {[
            {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
        ]}}
    ]}},
    {<<"shows">>, {[
        {<<"baz">>, <<"function(doc, req) {return '<h1>wosh</h1>';}">>}
    ]}},
    {<<"lists">>, {[
        {<<"fum">>, <<"
            function(head, req) {
              var row
              var rows=[]
              while(row = getRow()) {
                rows.push(row)
              }
              rows.sort(function(a,b) {
                return b.value-a.value
              })
              send(JSON.stringify({'rows' : rows}))
            }
        ">>}
    ]}}
]}).

setup(PortType) ->
    DbName = ?tempdb(),
    ok = create_db(PortType, DbName),

    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    DbUrl = "http://" ++ Addr ++ ":" ++ port(PortType) ++ "/" ++ ?b2l(DbName),
    upload_ddoc(DbUrl),
    {DbUrl, ?b2l(DbName)}.

teardown(PortType, {_Url, DbName}) ->
    delete_db(PortType, ?l2b(DbName)),
    ok.


mrview_etag_generation_test_() ->
    {
        "Etag generation for mrview",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end,
            fun test_util:stop_couch/1,
            [view_tests(), show_tests(), list_tests()]
        }
    }.

view_tests() ->
    Funs = [
        fun should_produce_same_etag_when_post_to_view_same_keys/2,
        fun should_produce_diff_etag_when_post_to_view_diff_keys/2,
        fun should_produce_same_etag_when_post_to_view_same_args/2,
        fun should_produce_diff_etag_when_post_to_view_diff_args/2,
        fun should_produce_diff_etag_when_post_to_updated_db/2,
        fun should_produce_same_etag_when_all_docs_same_args/2,
        fun should_produce_diff_etag_when_all_docs_diff_args/2,
        fun should_produce_diff_etag_when_all_docs_updated_db/2,
        fun ensure_no_etag_for_multiquery/2
    ],
    {
        "View etag generation tests",
        [
            make_test_case(clustered, Funs),
            make_test_case(backdoor, Funs)
        ]
    }.

show_tests() ->
    Funs = [
        fun should_produce_same_etag_when_get_show_same_args/2,
        fun should_produce_diff_etag_when_get_show_diff_args/2
    ],
    {
        "Show etag generation tests",
        [
            make_test_case(clustered, Funs),
            make_test_case(backdoor, Funs)
        ]
    }.

list_tests() ->
    Funs = [
        fun should_produce_same_etag_when_get_list_same_args/2,
        fun should_produce_diff_etag_when_get_list_diff_args/2,
        fun should_produce_diff_etag_when_get_list_updated_db/2
    ],
    {
        "View list etag generation tests",
        [
            make_test_case(clustered, Funs),
            make_test_case(backdoor, Funs)
        ]
    }.


make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_produce_same_etag_when_post_to_view_same_keys(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body = couch_util:json_encode({[
        {<<"keys">>, ["baz"]}
    ]}),
    ReqFun = fun() -> test_request:post(Url, Body) end,
    ?_assertEqual(request_etag(ReqFun), request_etag(ReqFun)).

should_produce_diff_etag_when_post_to_view_diff_keys(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body1 = couch_util:json_encode({[
        {<<"keys">>, ["baz"]}
    ]}),
    ReqFun1 = fun() -> test_request:post(Url, Body1) end,
    Body2 = couch_util:json_encode({[
        {<<"keys">>, ["qux"]}
    ]}),
    ReqFun2 = fun() -> test_request:post(Url, Body2) end,
    ?_assertNotEqual(request_etag(ReqFun1), request_etag(ReqFun2)).

should_produce_diff_etag_when_post_to_updated_db(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body = couch_util:json_encode({[
        {<<"keys">>, ["baz"]}
    ]}),
    ReqFun = fun() -> test_request:post(Url, Body) end,
    Etag1 = request_etag(ReqFun),
    update_db(DbUrl),
    ?_assertNotEqual(Etag1, request_etag(ReqFun)).

should_produce_same_etag_when_post_to_view_same_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body = couch_util:json_encode({[
        {<<"keys">>, ["baz"]}
    ]}),
    Args = [{update_seq, true}],
    ReqFun = fun() -> test_request:post(append_args(Url, Args), Body) end,
    ?_assertEqual(request_etag(ReqFun), request_etag(ReqFun)).

should_produce_diff_etag_when_post_to_view_diff_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body = couch_util:json_encode({[
        {<<"keys">>, ["baz"]}
    ]}),
    Args1 = [{update_seq, true}],
    ReqFun1 = fun() -> test_request:post(append_args(Url, Args1), Body) end,
    Args2 = [{update_seq, false}],
    ReqFun2 = fun() -> test_request:post(append_args(Url, Args2), Body) end,
    ?_assertNotEqual(request_etag(ReqFun1), request_etag(ReqFun2)).

ensure_no_etag_for_multiquery(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_view/bar",
    Body = couch_util:json_encode({[
        {<<"queries">>, [{[
           {<<"startkey">>, 1},
           {<<"limit">>, 2}
        ]}]}
    ]}),
    {ok, Code, Headers, _Body} = test_request:post(Url, Body),
    ?assertEqual(200, Code),
    Etag = couch_util:get_value("Etag", Headers),
    ?_assertEqual(undefined, Etag).

should_produce_same_etag_when_get_show_same_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_show/baz",
    Args = [{parrot, true}],
    ReqFun = fun() -> test_request:get(append_args(Url, Args), [], []) end,
    ?_assertEqual(request_etag(ReqFun), request_etag(ReqFun)).

should_produce_diff_etag_when_get_show_diff_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_show/baz",
    Args1 = [{update_seq, true}],
    ReqFun1 = fun() -> test_request:get(append_args(Url, Args1), [], []) end,
    Args2 = [{parrot, false}],
    ReqFun2 = fun() -> test_request:get(append_args(Url, Args2), [], []) end,
    ?_assertNotEqual(request_etag(ReqFun1), request_etag(ReqFun2)).

should_produce_same_etag_when_get_list_same_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_list/fum/bar",
    Args = [{parrot, true}],
    ReqFun = fun() -> test_request:get(append_args(Url, Args), [], []) end,
    ?_assertEqual(request_etag(ReqFun), request_etag(ReqFun)).

should_produce_diff_etag_when_get_list_diff_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_list/fum/bar",
    Args1 = [{parrot, true}],
    ReqFun1 = fun() -> test_request:get(append_args(Url, Args1), [], []) end,
    Args2 = [{parrot, false}],
    ReqFun2 = fun() -> test_request:get(append_args(Url, Args2), [], []) end,
    ?_assertNotEqual(request_etag(ReqFun1), request_etag(ReqFun2)).

should_produce_diff_etag_when_get_list_updated_db(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_design/foo/_list/fum/bar",
    ReqFun = fun() -> test_request:get(Url, [], []) end,
    Etag1 = request_etag(ReqFun),
    update_db(DbUrl),
    ?_assertNotEqual(Etag1, request_etag(ReqFun)).

should_produce_same_etag_when_all_docs_same_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_all_docs",
    Args = [{update_seq, true}],
    ReqFun = fun() -> test_request:get(append_args(Url, Args), [], []) end,
    Etag1 = request_etag(ReqFun),
    update_db(DbUrl),
    ?_assertNotEqual(Etag1, request_etag(ReqFun)).

should_produce_diff_etag_when_all_docs_diff_args(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_all_docs",
    Args1 = [{update_seq, true}],
    ReqFun1 = fun() -> test_request:get(append_args(Url, Args1), [], []) end,
    Args2 = [{update_seq, false}],
    ReqFun2 = fun() -> test_request:get(append_args(Url, Args2), [], []) end,
    ?_assertNotEqual(request_etag(ReqFun1), request_etag(ReqFun2)).

should_produce_diff_etag_when_all_docs_updated_db(_, {DbUrl, _DbName}) ->
    Url = DbUrl ++ "/_all_docs",
    ReqFun = fun() -> test_request:get(Url, [], []) end,
    Etag1 = request_etag(ReqFun),
    update_db(DbUrl),
    ?_assertNotEqual(Etag1, request_etag(ReqFun)).

create_db(backdoor, DbName) ->
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db);
create_db(clustered, DbName) ->
    ok = fabric:create_db(DbName, [?ADMIN_CTX]).

delete_db(backdoor, DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]);
delete_db(clustered, DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).

upload_ddoc(DbUrl) ->
    Url = DbUrl ++ "/_design/foo",
    Body = couch_util:json_encode(?DDOC),
    {ok, 201, _Resp, _Body} = test_request:put(Url, Body),
    ok.

update_db(Url) ->
    Headers = [{"Content-Type", "application/json"}],
    {ok, 201, _Resp, _Body} = test_request:post(Url, Headers, "{}"),
    ok.

append_args(Url, []) ->
    Url;
append_args(Url, Args) ->
    Url ++ "?" ++ mochiweb_util:urlencode(Args).

request_etag(ReqFun) ->
    {ok, Code, Headers, _Body} = ReqFun(),
    ?assertEqual(200, Code),
    Etag = couch_util:get_value("Etag", Headers),
    ?assert(is_list(Etag)),
    ?assertNotEqual(Etag, ""),
    ?assert(is_quoted(Etag)),
    Etag.

is_quoted([$"|_] = Etag) ->
    lists:last(Etag) == $";
is_quoted(_) ->
    false.
