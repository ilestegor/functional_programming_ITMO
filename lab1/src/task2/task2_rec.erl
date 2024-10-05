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
-export([fib_even_sum_rec/3]).

fib_even_sum_rec(X, Y, MaxFibNum) when X >= 0, Y >= 1, MaxFibNum >= 0 ->
    case X of
        _ when X > MaxFibNum -> 0;
        _ when X rem 2 == 0 -> X + fib_even_sum_rec(Y, X + Y, MaxFibNum);
        _ -> fib_even_sum_rec(Y, X + Y, MaxFibNum)
    end.
