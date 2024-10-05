%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2024 7:30 PM
%%%-------------------------------------------------------------------
-module(task29_test).
-author("ilestegor").

-include_lib("eunit/include/eunit.hrl").

task29_test_() ->
    [
        {"task29_map_test",
            ?_assertEqual(9183, task29_map:find_length_of_unique_powers(2, 100))},
        {"task29_module_test", ?_assertEqual(9183, task29_module:solve(2, 100))},
        {"task29_rec_test", ?_assertEqual(9183, task29_rec:solve(2, 100, 2, 100))},
        {"task29_simple_test", ?_assertEqual(9183, task29_simple:find_distinct(2, 100))},
        {"task29_tail_rec_test",
            ?_assertEqual(9183, task29_tail_rec:find_unique_tail_rec(2, 100, 2, 100))}
    ].
