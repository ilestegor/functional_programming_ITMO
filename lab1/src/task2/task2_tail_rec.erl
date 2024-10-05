%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2024 4:10 PM
%%%-------------------------------------------------------------------
-module(task2_tail_rec).

%% API
-export([fib_even_sum/3]).

fib_even_sum(X, Y, MaxFibNum) when X >= 0, Y >= 1, MaxFibNum > 0 ->
    fib_even_sum(X, Y, 0, MaxFibNum).
fib_even_sum(X, _Y, Acc, MaxFibNum) when X > MaxFibNum ->
    Acc;
fib_even_sum(X, Y, Acc, MaxFibNum) when X rem 2 == 0 ->
    fib_even_sum(Y, X + Y, Acc + X, MaxFibNum);
fib_even_sum(X, Y, Acc, MaxFibNum) ->
    fib_even_sum(Y, X + Y, Acc, MaxFibNum).
