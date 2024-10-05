%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2024 11:50 PM
%%%-------------------------------------------------------------------
-module(task29_module).

%% API
-export([generate_powers_list/2, solve/2]).

generate_powers_list(Left, Right) ->
    [round(math:pow(X, Y)) || X <- lists:seq(Left, Right), Y <- lists:seq(Left, Right)].

keep_unique(Elem) ->
    case get(Elem) of
        true ->
            false;
        undefined ->
            put(Elem, true),
            true
    end.

remove_duplicates(List) ->
    lists:filter(fun(X) -> keep_unique(X) end, List).

solve(Left, Right) ->
    lists:foldl(
        fun(_X, Acc) -> Acc + 1 end,
        0,
        lists:sort(remove_duplicates(generate_powers_list(Left, Right)))
    ).
