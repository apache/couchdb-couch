-module(couch_dbs).

-behaviour(couch_define_db).

-export([databases/0, validate_name/1, options/1, name/1]).

databases() ->
    [name("authentication_db")].

validate_name(DbName) ->
    name("authentication_db") == couch_db:normalize_dbname(DbName).

options(_Name) ->
    [
        {before_doc_update, fun couch_users_db:before_doc_update/2},
        {after_doc_read, fun couch_users_db:after_doc_read/2},
        sys_db,
        nologifmissing,
        local
    ].

name("authentication_db") ->
    config:get("couch_httpd_auth", "authentication_db", "_users").
