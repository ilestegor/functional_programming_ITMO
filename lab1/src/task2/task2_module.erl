%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2024 11:47 PM
%%%-------------------------------------------------------------------
-module(task2_module).

%% API
-export([start/3, generate_fib/3]).

generate_fib(X, _Y, MaxNum) when X >= MaxNum -> [];
generate_fib(X, Y, MaxNum) -> [X | generate_fib(Y, X + Y, MaxNum)].

filter_seq(_, [], Acc) ->
    Acc;
filter_seq(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter_seq(Pred, T, [H | Acc]);
        false -> filter_seq(Pred, T, Acc)
    end.

fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(Start, H), T).

sum(X, Y) -> X + Y.

is_even(X) -> X rem 2 == 0.

start(X, Y, MaxFibNum) ->
    FibNums = generate_fib(X, Y, MaxFibNum),
    EvenFibList = filter_seq(fun is_even/1, FibNums, []),
    fold(fun sum/2, 0, EvenFibList).
