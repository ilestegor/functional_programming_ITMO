%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2024 5:19 PM
%%%-------------------------------------------------------------------
-module(hashmap_unit_test).
-author("ilestegor").

-include_lib("eunit/include/eunit.hrl").
-include("../src/hashmap_dict.hrl").

hashmap_test_() ->
    [
        {"creation_test", create_test()},
        {"create_with_capacity_test", create_with_capacity_test()},
        {"resize_test", resize_test()},
        {"update_value", insert_update_value_test()},
        {"get_non_existent_key", get_non_existent_key()},
        {"remove_existent_elemnt_test}", remove_existent_element()},
        {"remove_non_existent_elemnt_test}", remove_non_existent_element()},
        {"foldl_test", foldl_test()}
    ].

create_test() ->
    D = hashmap_dict:new(),
    ?_assertEqual(0, D#oahashdict.size),
    ?_assertEqual(10, D#oahashdict.capacity).

create_with_capacity_test() ->
    D = hashmap_dict:new(50),
    ?_assertEqual(50, D#oahashdict.capacity),
    ?_assertEqual(0, D#oahashdict.size).

resize_test() ->
    D = hashmap_dict:new(1),
    NewD = hashmap_dict:insert(hashmap_dict:insert(D, 1, 2), 3, 4),
    ?_assertEqual(2, NewD#oahashdict.capacity),
    ?_assertEqual(2, NewD#oahashdict.size).

insert_update_value_test() ->
    Dict = hashmap_dict:new(),
    Dict1 = hashmap_dict:insert(Dict, "key", "value1"),
    Dict2 = hashmap_dict:insert(Dict1, "key", "value2"),
    ?_assertEqual(1, Dict2#oahashdict.size).

get_non_existent_key() ->
    Dict = hashmap_dict:new(),
    Dict1 = hashmap_dict:insert(Dict, 1, 2),
    Value = hashmap_dict:get(Dict1, 2),
    ?_assertEqual(not_found, Value).

remove_existent_element() ->
    Dict = hashmap_dict:new(),
    Dict1 = hashmap_dict:insert(Dict, "thank you", "beyonce"),
    NewDict = hashmap_dict:delete(Dict1, "thank you"),
    ?_assertEqual(0, NewDict#oahashdict.size).

remove_non_existent_element() ->
    Dict = hashmap_dict:new(),
    Dict1 = hashmap_dict:insert(Dict, "thank you", "beyonce"),
    NewDict = hashmap_dict:delete(Dict1, "thank"),
    ?_assertEqual(false, NewDict).

foldl_test() ->
    Dict = hashmap_dict:new(),
    NewDict = hashmap_dict:insert(hashmap_dict:insert(Dict, 1, 2), 3, 4),
    Result = hashmap_dict:foldl(NewDict, 0, fun(X, {_, V}) -> V + X end),
    ?_assertEqual(6, Result).









