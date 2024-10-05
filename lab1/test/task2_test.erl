%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2024 6:45 PM
%%%-------------------------------------------------------------------
-module(task2_test).
-author("ilestegor").

-include_lib("eunit/include/eunit.hrl").

task2_test_() ->
    [
        {"task2_map_test", ?_assertEqual(4613732, task2_map:start(0, 1, 4000000))},
        {"task2_map_until_test",
            ?_assertEqual(
                4613732, task2_map_until:generate_fib_list_map(0, 100, 4000000)
            )},
        {"task2_module_test", ?_assertEqual(4613732, task2_module:start(0, 1, 4000000))},
        {"task2_module_bif_test",
            ?_assertEqual(4613732, task2_module_bif:start(0, 1, 4000000))},
        {"tasl2_rec_test",
            ?_assertEqual(4613732, task2_rec:fib_even_sum_rec(0, 1, 4000000))},
        {"task2_tail_rec_test",
            ?_assertEqual(4613732, task2_tail_rec:fib_even_sum(0, 1, 4000000))}
    ].
