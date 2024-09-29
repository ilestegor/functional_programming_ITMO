%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2024 11:47 PM
%%%-------------------------------------------------------------------
-module(task2_module).
-author("ilestegor").

%% API
-export([start/0, generate_fib/3]).
-compile(nowarn_unused_function).
-define(MAX_FIB_NUM, 4000000).

%%generate fib sequence
generate_fib(X, _Y, MaxNum) when X >= MaxNum -> [];
generate_fib(X, Y, MaxNum) -> [X | generate_fib(Y, X + Y, MaxNum)].

%%filter sequence
filter_seq(_, [], Acc) -> Acc;
filter_seq(Pred, [H|T], Acc) ->
  case Pred(H) of
    true -> filter_seq(Pred, T, [H|Acc]);
    false -> filter_seq(Pred, T, Acc)
  end.

%%fold
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(Start, H), T).

sum(X, Y) -> X + Y.

is_even(X) -> X rem 2 == 0.


start() ->
  FibNums = generate_fib(0, 1, ?MAX_FIB_NUM),
  EvenFibList = filter_seq(fun is_even/1, FibNums, []),
  Sum = fold(fun sum/2, 0, EvenFibList),
  io:format("~s ~p ~s ~p", ["Sum of even-valued Fibonacci terms that do not exceed", ?MAX_FIB_NUM, " -- ", Sum]).
