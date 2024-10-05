%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2024 10:21 PM
%%%-------------------------------------------------------------------
-module(task29_rec).

%% API
-export([pow/2, iterate_base/4, solve/4]).
pow(_, 0) -> 1;
pow(X, Y) when Y >= 1 -> X * pow(X, Y - 1).

solve(Base, MaxBase, Power, MaxPower) ->
    length(lists:usort(iterate_base(Base, MaxBase, Power, MaxPower))).

iterate_base(Base, MaxBase, _Power, _MaxPower) when Base > MaxBase -> [];
iterate_base(Base, MaxBase, Power, MaxPower) ->
    generate_powers(Base, Power, MaxPower) ++
        iterate_base(Base + 1, MaxBase, Power, MaxPower).

generate_powers(_, Power, MaxPower) when Power > MaxPower -> [];
generate_powers(Base, Power, MaxPower) ->
    [pow(Base, Power) | generate_powers(Base, Power + 1, MaxPower)].
