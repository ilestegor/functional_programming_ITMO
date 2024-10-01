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
-export([fib_even_sum/2, solve/0]).
-define(MAX_FIB_NUM, 4000000).

solve() when ?MAX_FIB_NUM >= 0 ->
    fib_even_sum(0, 1).

fib_even_sum(X, Y) when X >= 0, Y >= 1 ->
    fib_even_sum(X, Y, 0).

fib_even_sum(X, _Y, Acc) when X > ?MAX_FIB_NUM ->
    Acc;
fib_even_sum(X, Y, Acc) when X rem 2 == 0 ->
    fib_even_sum(Y, X + Y, Acc + X);
fib_even_sum(X, Y, Acc) ->
    fib_even_sum(Y, X + Y, Acc).
