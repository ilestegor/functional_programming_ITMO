%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2024 4:06 PM
%%%-------------------------------------------------------------------
-module(task2_rec).

%% API
-export([fib_even_sum_rec/0]).
-define(MAX_FIB_NUM, 4000000).

fib_even_sum_rec(X, Y) when X >= 0, Y >= 1, ?MAX_FIB_NUM >= 0 ->
    case X of
        _ when X > ?MAX_FIB_NUM -> 0;
        _ when X rem 2 == 0 -> X + fib_even_sum_rec(Y, X + Y);
        _ -> fib_even_sum_rec(Y, X + Y)
    end.
fib_even_sum_rec() ->
    fib_even_sum_rec(0, 1).
