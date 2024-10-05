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
-export([start/3]).

start(K, Z, MaxFibNum) ->
    FibList = task2_module:generate_fib(K, Z, MaxFibNum),
    EvenFibList = lists:filter(fun(X) -> X rem 2 == 0 end, FibList),
    lists:foldl(fun(X, Y) -> X + Y end, 0, EvenFibList).
