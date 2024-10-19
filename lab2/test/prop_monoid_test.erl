%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2024 2:50 PM
%%%-------------------------------------------------------------------
-module(prop_monoid_test).
-author("ilestegor").

-include_lib("proper/include/proper.hrl").
-include("../src/hashmap_dict.hrl").

prop_associativity_test() ->
    ?FORALL(
        {Tuples1, Tuples2, Tuples3},
        {list({int(), int()}), list({int(), int()}), list({int(), int()})},
        begin
            Dict1 = hashmap_dict:new(),
            FilledDict1 = fill_dict(Dict1, Tuples1),
            Dict2 = hashmap_dict:new(),
            FilledDict2 = fill_dict(Dict2, Tuples2),
            Dict3 = hashmap_dict:new(),
            FilledDict3 = fill_dict(Dict3, Tuples3),

            Merge1 = hashmap_dict:combine(
                hashmap_dict:combine(FilledDict1, FilledDict2), FilledDict3
            ),
            Merge2 = hashmap_dict:combine(
                hashmap_dict:combine(FilledDict2, FilledDict3), FilledDict1
            ),
            is_equal(Merge1, Merge2) == true
        end
    ).

prop_neutral() ->
    ?FORALL(
        {Tuple},
        {list({int(), int()})},
        begin
            Empty = hashmap_dict:empty_map(),
            Dict1 = hashmap_dict:new(),
            FilledDict = fill_dict(Dict1, Tuple),

            Merge1 = hashmap_dict:combine(Empty, FilledDict),
            Merge2 = hashmap_dict:combine(FilledDict, Empty),
            is_equal(Merge1, Merge2),
            is_equal(Merge1, FilledDict)
        end
    ).

prop_insert_test() ->
    ?FORALL(
        {Key, Value},
        {int(), int()},
        begin
            Dict = hashmap_dict:new(),
            UpdatedDict = hashmap_dict:insert(Dict, Key, Value),

            case hashmap_dict:get(UpdatedDict, Key) of
                Value -> true;
                _ -> false
            end
        end
    ).

fill_dict(Dict, []) -> Dict;
fill_dict(Dict, [{K, V} | T]) -> fill_dict(hashmap_dict:insert(Dict, K, V), T).

is_equal(#oahashdict{table = T1}, #oahashdict{} = Dict2) ->
    check_equal(T1, Dict2).

check_equal([], _) ->
    true;
check_equal([{none, _} | T], Dict2) ->
    check_equal(T, Dict2);
check_equal([{K, V} | T], Dict2) ->
    case hashmap_dict:get(Dict2, K) of
        V -> check_equal(T, Dict2);
        _ -> false
    end.
