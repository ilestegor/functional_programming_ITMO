%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2024 12:27 PM
%%%-------------------------------------------------------------------
-module(task2_map).

%% API

-export([start/3]).

generate_fib_list_map(Z, K, MaxFibNum) ->
    FibList = task2_module:generate_fib(Z, K, MaxFibNum),
    MappedList = lists:map(
        fun
            (X) when X rem 2 == 0 -> X;
            (_) -> false
        end,
        FibList
    ),
    lists:filter(fun(X) -> X /= false end, MappedList).

start(Z, K, MaxFibNum) ->
    lists:foldl(fun(X, Y) -> X + Y end, 0, generate_fib_list_map(Z, K, MaxFibNum)).
