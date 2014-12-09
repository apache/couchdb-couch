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

-module(couchdb_compaction_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(ATT_TXT_NAME, <<"test_file.txt">>).


start() ->
    ok = test_util:start_couch(),
    ok.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    couch_db:close(Db),

    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    {Host, ?b2l(DbName)}.

teardown({_, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(?l2b(DbName), []),
    ok.

attachments_test_() ->
    {
        "Attachments tests",
        {
            setup,
            fun start/0, fun test_util:stop_couch/1,
            [
                compaction_tests()
            ]
        }
    }.

compaction_tests() ->
    [
        {
            "Attachment compaction tests",
            [
                {
                    foreach,
                    fun setup/0, fun teardown/1,
                    [
                        fun should_not_duplicate_inline_atts/1
                    ]
                }
            ]
        }
    ].

should_not_duplicate_inline_atts({Host, DbName}) ->
    ?_test(begin
        create_inline_text_att(DbName, <<"doc1">>),
        create_inline_text_att(DbName, <<"doc2">>),
        {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
        ?assertEqual(2, count_unique_atts(DbName)),
        couch_db:start_compact(Db),
        couch_db:wait_for_compaction(Db),
        ?assertEqual(2, count_unique_atts(DbName))
    end).

create_inline_text_att(DbName, Id) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    Doc = doc_with_attach(Id),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Rev.

count_unique_atts(DbName) ->
    {ok, Db} = couch_db:open_int(list_to_binary(DbName), []),
    {ok, _, Atts} = couch_btree:foldl(Db#db.id_tree,
        fun(#full_doc_info{rev_tree = Tree}, Acc) ->
            Atts = couch_key_tree:fold(
                fun(_, #leaf{atts = P}, _, A) -> [P|A] end, [], Tree),
            {ok, Atts ++ Acc}
        end, []),
    couch_db:close(Db),
    sets:size(sets:from_list(Atts)).

doc_with_attach(Id) ->
    EJson = body_with_attach(Id),
    couch_doc:from_json_obj(EJson).

body_with_attach(Id) ->
    Data = <<"Test file content">>,
    {[
        {<<"_id">>, Id},
        {<<"_attachments">>, {[
            {?ATT_TXT_NAME, {[
                {<<"content_type">>, <<"text/plain">>},
                {<<"data">>, base64:encode(Data)}
            ]}
        }]}}
    ]}.
