%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2024 12:28 AM
%%%-------------------------------------------------------------------
-module(task2_module_bif).

%% API
-export([start/0]).
-define(MAX_FIB_NUM, 4000000).

%%with BIF and anon functions

start() ->
    FibList = task2_module:generate_fib(0, 1, ?MAX_FIB_NUM),
    EvenFibList = lists:filter(fun(X) -> X rem 2 == 0 end, FibList),
    lists:foldl(fun(X, Y) -> X + Y end, 0, EvenFibList).
