-module(couch_define_db).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {databases, 0},
        {validate_name, 1},
        {options, 1},
        {name, 1}
    ].
