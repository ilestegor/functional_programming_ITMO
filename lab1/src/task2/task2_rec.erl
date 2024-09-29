%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2024 4:06 PM
%%%-------------------------------------------------------------------
-module(task2_rec).
-author("ilestegor").

%% API
-export([fib_even_sum_rec/0]).
-define(MAX_FIB_NUM, 4000000).



fib_even_sum_rec(X, Y) when X >= 0, Y >= 1, ?MAX_FIB_NUM >= 0 ->
  if
    X > ?MAX_FIB_NUM -> 0; X rem 2 == 0 -> X + fib_even_sum_rec(Y, X + Y);
    true -> fib_even_sum_rec(Y, X + Y)
  end.
fib_even_sum_rec() ->
  io:format("~s ~p ~s ~p", ["Sum of even-valued Fibonacci terms that do not exceed", ?MAX_FIB_NUM, " -- ", fib_even_sum_rec(0, 1)]).
