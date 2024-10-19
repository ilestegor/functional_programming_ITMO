# Lab2

- Выполнил: Глотов Егор Дмитриевич
- Группа: P3332
- ИСУ: 368030

### Цель
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

### Задание
Реализовать выданную структуру данных -- `Open Addressing HashMap Dict`

### Требования
1. Функции:
   - добавление и удаление элементов;
   - фильтрация;
   - отображение (map);
   - свертки (левая и правая);
   - структура должна быть моноидом.

2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

### Ключевые элементы реализации

Вид структуры в коде

```erlang
-record(oahashdict, {
  table = [],
  size = 0,
  capacity = 10
}).
```

Вставка элемента в структуру. При вставке также идет проверка на превышение
допустимой заполненности (load_factor). При превышении этого коэффициента
структура расширяется в два раща

```erlang
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
```

Линейное пробирование для разрешения коллизий при вставке нового элемента

```erlang
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
```

Левая и правая свертка. Правая свертка основана на левой (происходит
переворачивание массива и вызов левой свертки)

```erlang
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
```

Слияние двух структур. Для начала мы ищем одинаковые ключи и складываем их значения. Как только одинаковых ключей нет мы переходим к тому, что берем все оставшиеся элементы 
из двух структур и перекладываем в искомую

```erlang
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
```

Удаление элемента из структуры

```erlang
delete(#oahashdict{table = Table, size = Size} = Dict, Key) ->
    Idx = contains_key(Dict, Key),
    case Idx of
        false ->
            false;
        _ ->
            UpdatedTable = replace_term(Idx + 1, {none, none}, Table),
            Dict#oahashdict{table = UpdatedTable, size = Size - 1}
    end.
```

Фильтрация и маппинг для структуры

```erlang
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

map(Pred, #oahashdict{table = Table, capacity = Capacity}) when Capacity > 0 ->
    map_seq(Pred, Table).

map_seq(_, []) ->
    [];
map_seq(Pred, [H | T]) ->
    case H of
        {K, V} when K =/= none -> [{K, Pred(V)} | map_seq(Pred, T)];
        _ -> map_seq(Pred, T)
    end.
```

Для обеспечения того, чтобы структура была моноидом определим нейтральный элемент. В нашем случае это будет 
пустая структура с `capacity = 0` и `size = 0`. При слиянии двух непустых структур значения, у которых одинаковые ключи, будем
складывать

### Тестирование
Тестирование проводилось при помощи двух инструментов:
 - Eunit -- для Unit-тестирования
 - rebar3 proper -- для property-based тестирования

### Вывод
В результате выполнения лабораторной работы мне удалось реализовать данную
структуру данных, а также свои реализации функций map, filter, foldl, foldr, что было достаточно просто 
сделать на Erlang. Сама реализация структуры данных оказалась несколько сложнее из-за различных мелочей, которые
всплывали по мере написания кода и тестов. Также я познакомился с property-based тестирование, что оказалось
достаточно эффективным (благодаря ему повылезали некоторые недочеты, которые изначально я не заметил)