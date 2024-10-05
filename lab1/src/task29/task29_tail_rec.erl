%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2024 3:37 PM
%%%-------------------------------------------------------------------
-module(task29_tail_rec).

%% API
-export([pow/2, find_unique_tail_rec/4, iterate_base/5]).

pow(X, Y) -> tail_rec_pow(X, Y, 1).

tail_rec_pow(_, 0, Acc) -> Acc;
tail_rec_pow(X, Y, Acc) -> tail_rec_pow(X, Y - 1, Acc * X).

iterate_base(Base, MaxBase, Acc, _Power, _MaxPower) when Base > MaxBase ->
    lists:reverse(Acc);
iterate_base(Base, MaxBase, Acc, Power, MaxPower) ->
    NewPowers = generate_powers(Base, Power, MaxPower, []),
    iterate_base(Base + 1, MaxBase, NewPowers ++ Acc, Power, MaxPower).

generate_powers(_, Power, MaxPower, Acc) when Power > MaxPower ->
    lists:reverse(Acc);
generate_powers(Base, Power, MaxPower, Acc) ->
    generate_powers(Base, Power + 1, MaxPower, [pow(Base, Power) | Acc]).

find_unique_tail_rec(Base, MaxBase, Power, MaxPower) ->
    length(lists:usort(iterate_base(Base, MaxBase, [], Power, MaxPower))).
