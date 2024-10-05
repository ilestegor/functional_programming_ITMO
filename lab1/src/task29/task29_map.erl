%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2024 2:32 PM
%%%-------------------------------------------------------------------
-module(task29_map).

%% API
-export([generate_list/2, generate_powers/2, find_length_of_unique_powers/2]).

generate_list(Left, Right) ->
    [{X, Y} || X <- lists:seq(Left, Right), Y <- lists:seq(Left, Right)].

generate_powers(Left, Right) ->
    lists:map(fun({X, Y}) -> round(math:pow(X, Y)) end, generate_list(Left, Right)).

find_length_of_unique_powers(Left, Right) ->
    length(lists:usort(generate_powers(Left, Right))).
