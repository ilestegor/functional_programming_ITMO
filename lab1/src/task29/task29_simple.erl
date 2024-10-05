%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2024 11:54 PM
%%%-------------------------------------------------------------------
-module(task29_simple).

%% API
-export([find_distinct/2]).

find_distinct(Left, Right) ->
    length(
        lists:usort([
            math:pow(X, Y)
         || X <- lists:seq(Left, Right), Y <- lists:seq(Left, Right)
        ])
    ).
