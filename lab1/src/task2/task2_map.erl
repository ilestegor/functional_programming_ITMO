%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2024 12:27 PM
%%%-------------------------------------------------------------------
-module(task2_map).
-author("ilestegor").

%% API


-export([start/0]).
-define(MAX_FIB_NUM, 4000000).

generate_fib_list_map() ->
  FibList = task2_module:generate_fib(0, 1, ?MAX_FIB_NUM),
  MappedList = lists:map(fun(X) when X rem 2 == 0 -> X; (_) -> false end, FibList),
  lists:filter(fun(X) -> X /= false end, MappedList).


start() -> io:format("~p", [lists:foldl(fun(X, Y) -> X + Y end, 0, generate_fib_list_map())]).
