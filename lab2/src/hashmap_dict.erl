%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2024 5:54 PM
%%%-------------------------------------------------------------------
-module(hashmap_dict).

%% API
-export([
    new/1, new/0,
    insert/3,
    start/0,
    foldl/3,
    contains_key/2,
    get/2,
    foldr/3,
    delete/2,
    filter/3,
    map/2,
    empty_map/0,
    combine/2
]).
-include("hashmap_dict.hrl").
-define(LOAD_FACTOR, 0.75).
-define(GROW_FACTOR, 2).

new() ->
    new(10).

new(Capacity) ->
    EmptyTable = lists:duplicate(Capacity, {none, none}),
    #oahashdict{table = EmptyTable, size = 0, capacity = Capacity}.

insert(#oahashdict{table = Table, size = Size, capacity = Capacity} = Dict, Key, Value) ->
    case (Size / Capacity > ?LOAD_FACTOR) of
        true ->
            NewDict = resize(Dict),
            insert(NewDict, Key, Value);
        false ->
            Index = linear_probing(Dict, Key, erlang:phash2(Key) rem Capacity),
            UpdatedTable = replace_term(Index + 1, {Key, Value}, Table),
            case lists:nth(Index + 1, Table) of
                {Key, _} ->
                    Dict#oahashdict{table = UpdatedTable};
                _ ->
                    Dict#oahashdict{table = UpdatedTable, size = Size + 1}
            end
    end.

resize(#oahashdict{table = _Table, capacity = Capacity} = Dict) ->
    NewCapacity = Capacity * ?GROW_FACTOR,
    NewTable = lists:duplicate(NewCapacity, {none, none}),
    NewDict = Dict#oahashdict{table = NewTable, size = 0, capacity = NewCapacity},
    foldl(Dict, NewDict, fun rehash/2).

rehash(Dict, {none, none}) -> Dict;
rehash(Dict, {K, V}) when K =/= none -> insert(Dict, K, V).

linear_probing(
    #oahashdict{table = Table, size = Size, capacity = Capacity},
    Key,
    Index
) ->
    case lists:nth(Index + 1, Table) of
        {Key, _} ->
            Index;
        {none, none} ->
            Index;
        _ ->
            linear_probing(
                #oahashdict{table = Table, size = Size, capacity = Capacity},
                Key,
                (Index + 1) rem Capacity
            )
    end.

replace_term(Index, Element, Table) ->
    {Front, [_ | Back]} = lists:split(Index - 1, Table),
    Front ++ [Element] ++ Back.

get(#oahashdict{table = Table} = Dict, Key) ->
    Idx = contains_key(Dict, Key),
    case Idx of
        false ->
            not_found;
        _ ->
            {_, V} = lists:nth(Idx + 1, Table),
            V
    end.

foldl(#oahashdict{table = Table, size = Size}, Acc, F) when Size > 0 ->
    foldl_tail_rec(Table, Acc, F).

foldl_tail_rec([], Acc, _) ->
    Acc;
foldl_tail_rec([{Key, Value} | T], Acc, F) ->
    case Key =/= none of
        true -> foldl_tail_rec(T, F(Acc, {Key, Value}), F);
        _ -> foldl_tail_rec(T, Acc, F)
    end.

foldr(#oahashdict{table = Table, size = Size}, Acc, F) when Size > 0 ->
    foldl_tail_rec(lists:reverse(Table), Acc, F).

contains_key(#oahashdict{table = Table, capacity = Capacity}, Key) ->
    case Capacity of
        0 ->
            false;
        _ ->
            Index = erlang:phash2(Key) rem Capacity,
            look_for_key(Table, Key, Index, Capacity, Index)
    end.

look_for_key(Table, Key, Index, Capacity, StartIndex) ->
    case lists:nth(Index + 1, Table) of
        {Key, _} when Key =/= none -> Index;
        {none, none} ->
            false;
        _ ->
            NewIndex = (Index + 1) rem Capacity,
            case NewIndex == StartIndex of
                true -> false;
                false -> look_for_key(Table, Key, NewIndex, Capacity, StartIndex)
            end
    end.

delete(#oahashdict{table = Table, size = Size} = Dict, Key) ->
    Idx = contains_key(Dict, Key),
    case Idx of
        false ->
            false;
        _ ->
            UpdatedTable = replace_term(Idx + 1, {none, none}, Table),
            Dict#oahashdict{table = UpdatedTable, size = Size - 1}
    end.

filter(Pred, #oahashdict{table = Table, capacity = Capacity}, Acc) when Capacity > 0 ->
    filter_seq(Pred, Table, Acc).

filter_seq(_, [], Acc) ->
    Acc;
filter_seq(Pred, [H | T], Acc) ->
    case H of
        {K, _} when K =/= none ->
            case Pred(H) of
                true -> filter_seq(Pred, T, [H | Acc]);
                false -> filter_seq(Pred, T, Acc)
            end;
        _ ->
            filter_seq(Pred, T, Acc)
    end.

map(Pred, #oahashdict{table = Table, capacity = Capacity, size = Size} = Dict) when
    Capacity > 0
->
    Dict#oahashdict{table = map_seq(Pred, Table), size = Size, capacity = Capacity}.

map_seq(_, []) ->
    [];
map_seq(Pred, [H | T]) ->
    case H of
        {K, V} when K =/= none -> [{K, Pred(V)} | map_seq(Pred, T)];
        _ -> map_seq(Pred, T)
    end.

empty_map() -> new(0).

combine(
    #oahashdict{table = Table1, size = S1, capacity = C1},
    #oahashdict{table = Table2, size = S2, capacity = C2} = Dict2
) ->
    NewDict = new(max(max(C1, C2), S1 + S2)),

    SameKeysMergedTable = combine_same_keys(Table1, Dict2, NewDict),
    combine_keys(Table2, SameKeysMergedTable, SameKeysMergedTable).

combine_same_keys([], _, Acc) ->
    Acc;
combine_same_keys([{none, _} | T], Dict2, Acc) ->
    combine_same_keys(T, Dict2, Acc);
combine_same_keys([{K, V} | T], Dict2, Acc) ->
    case get(Dict2, K) of
        not_found ->
            combine_same_keys(T, Dict2, insert(Acc, K, V));
        Value ->
            case Value of
                none -> combine_same_keys(T, Dict2, insert(Dict2, K, none));
                _ -> combine_same_keys(T, Dict2, insert(Acc, K, V + Value))
            end
    end.

combine_keys([], _, Acc) ->
    Acc;
combine_keys([{none, _} | T], Dict2, Acc) ->
    combine_keys(T, Dict2, Acc);
combine_keys([{K, V} | T], Dict2, Acc) ->
    case get(Dict2, K) of
        not_found ->
            combine_keys(T, Dict2, insert(Acc, K, V));
        Value ->
            case Value of
                none -> combine_keys(T, Dict2, insert(Dict2, K, V));
                _ -> combine_keys(T, Dict2, insert(Acc, K, Value))
            end
    end.

start() ->
    D = hashmap_dict:new(1),
    New = insert(insert(D, 1, 2), 3, 4),
    map(fun(X) -> X * 2 end, New).
