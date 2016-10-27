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

setup(Seq) ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_docs(DbName),
    ok = meck:new(couch_file, [passthrough]),
    ok = meck:expect(couch_file, close, ['_'], meck:seq(Seq)),
    DbName.

teardown(_, DbName) ->
    (catch meck:unload()),
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

compaction_recovery_test_() ->
    OK = [meck:passthrough()],
    SingleCrash = [
        meck:raise(error, something_bad),
        meck:passthrough()
    ],
    TwoCrashes = [
        meck:raise(error, something_bad),
        meck:raise(error, something_bad)
    ],

    TwoCrashesAndRecovery = [
        meck:raise(error, something_bad),
        meck:raise(error, something_bad),
        meck:passthrough()
    ],

    {
        "Compaction tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreachx,
                fun setup/1, fun teardown/2,
                [
                    {OK, fun should_compact/2},
                    {SingleCrash, fun should_reuse_files_after_first_crash/2},
                    {TwoCrashes, fun should_delete_files_after_second_crash/2},
                    {TwoCrashesAndRecovery, fun should_compact_from_begining/2}
                ]
            }
        }
    }.

should_reuse_files_after_first_crash(_, DbName) ->
    ?_test(begin
        {ok, Db0} = couch_db:open_int(DbName, []),
        {ok, CompactorPid0} = couch_db:start_compact(Db0),
        ?assertMatch({error, something_bad}, wait_compaction(CompactorPid0)),
        ?assert(compaction_files_exist(Db0)),
        wait_db_close(Db0),
        ?assert(filelib:is_regular(crash_file(Db0))),
        ?assert(compaction_files_exist(Db0)),

        {ok, Db1} = couch_db:open_int(DbName, []),
        {ok, CompactorPid1} = couch_db:start_compact(Db1),
        ?assertMatch(normal, wait_compaction(CompactorPid1)),
        ?assertNot(filelib:is_regular(crash_file(Db1))),
        ok
    end).

should_delete_files_after_second_crash(_, DbName) ->
    ?_test(begin
        {ok, Db0} = couch_db:open_int(DbName, []),
        {ok, CompactorPid0} = couch_db:start_compact(Db0),
        ?assertMatch({error, something_bad}, wait_compaction(CompactorPid0)),
        wait_db_close(Db0),
        ?assert(filelib:is_regular(crash_file(Db0))),
        ?assert(compaction_files_exist(Db0)),

        {ok, Db1} = couch_db:open_int(DbName, []),
        {ok, CompactorPid1} = couch_db:start_compact(Db1),
        ?assertMatch({error, something_bad}, wait_compaction(CompactorPid1)),
        ?assertNot(filelib:is_regular(crash_file(Db1))),
        ?assertNot(compaction_files_exist(Db1)),
        ok
    end).

should_compact_from_begining(_, DbName) ->
    ?_test(begin
        {ok, Db0} = couch_db:open_int(DbName, []),
        {ok, CompactorPid0} = couch_db:start_compact(Db0),
        ?assertMatch({error, something_bad}, wait_compaction(CompactorPid0)),
        wait_db_close(Db0),
        ?assert(filelib:is_regular(crash_file(Db0))),
        ?assert(compaction_files_exist(Db0)),

        {ok, Db1} = couch_db:open_int(DbName, []),
        {ok, CompactorPid1} = couch_db:start_compact(Db1),
        ?assertMatch({error, something_bad}, wait_compaction(CompactorPid1)),
        ?assertNot(filelib:is_regular(crash_file(Db1))),

        ?assertNot(compaction_files_exist(Db1)),

        {ok, Db2} = couch_db:open_int(DbName, []),
        {ok, CompactorPid2} = couch_db:start_compact(Db2),
        ?assertMatch(normal, wait_compaction(CompactorPid2)),
        ?assertNot(filelib:is_regular(crash_file(Db2))),
        ok
    end).

should_compact(_, DbName) ->
    ?_test(begin
        {ok, Db} = couch_db:open_int(DbName, []),
        {ok, CompactorPid} = couch_db:start_compact(Db),
        ?assertMatch(normal, wait_compaction(CompactorPid)),
        ?assertNot(filelib:is_regular(crash_file(Db))),
        ok
    end).


wait_compaction(CompactorPid) ->
    CompactorMonitor = erlang:monitor(process, CompactorPid),
    receive
        {'DOWN', CompactorMonitor, process, CompactorPid, Result} ->
            Result
    after 5000 ->
            exit(0)
    end.

wait_db_close(#db{main_pid = Pid}) ->
    test_util:wait(fun() ->
        case is_process_alive(Pid) of
            true -> wait;
            false -> ok
        end
    end).

crash_file(#db{filepath = Filepath}) ->
    Filepath ++ ".compact.crash".

compaction_files_exist(#db{filepath = Filepath}) ->
    lists:all(fun(Ext) ->
        filelib:is_regular(Filepath ++ Ext)
    end, [".compact.data", ".compact.meta"]).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_docs(Db, docs(3)),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

docs(N) ->
    [doc_body(Idx) || Idx <- lists:seq(1, N)].

doc_body(Idx) ->
    Id = "doc" ++ integer_to_list(Idx),
    couch_doc:from_json_obj({[
        {<<"_id">>, ?l2b(Id)},
        {<<"value">>, Idx}

    ]}).
