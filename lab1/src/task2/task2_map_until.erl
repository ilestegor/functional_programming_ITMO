%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2024 6:37 PM
%%%-------------------------------------------------------------------
-module(task2_map_until).

%% API
-export([generate_fib_list_map/3]).

map_until(_, [], _) ->
    [];
map_until(Fun, [H | T], MaxValue) ->
    Result = Fun(H),
    case Result > MaxValue of
        true -> [];
        false -> [Result | map_until(Fun, T, MaxValue)]
    end.

fib(N) when N >= 0 ->
    fib(N, 1, 1).

fib(0, A, _) -> A;
fib(N, A, B) -> fib(N - 1, B, A + B).

generate_fib_list_map(Left, Right, MaxFibNum) ->
    List = lists:seq(Left, Right),
    FibList = map_until(fun fib/1, List, MaxFibNum),
    lists:foldl(
        fun(X, Y) -> X + Y end,
        0,
        lists:filter(fun(X) -> X rem 2 == 0 end, FibList)
    ).
