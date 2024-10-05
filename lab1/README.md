# Lab1

- Выполнил: Глотов Егор Дмитриевич
- Группа: P3332
- ИСУ: 368030

## Задача 2

### Описание задачи
Каждое новое слагаемое в последовательности Фибоначчи генерируется путем сложения двух
предыдущих слагаемых. Если начинать с $1$ и $2$, то первые $10$ слагаемых будут:
$$1,2,3,5,8,13,21,34,55,89,...$$

Рассматривая слагаемые в последовательности Фибоначчи, значения которых не превышают четырех миллионов, найдите сумму
четных слагаемых.

### Решения задачи
1. Рекурсия

Используем рекурсию для генерации чисел Фибоначчи. Если четное число Фибоначчи найдено, то
прибавляем его с `X` и дальше рекурсивно продолжаем производить вычисления, если нет,
то просто вычисляем числа дальше.
```erlang
fib_even_sum_rec(X, Y, MaxFibNum) when X >= 0, Y >= 1, MaxFibNum >= 0 ->
    case X of
        _ when X > MaxFibNum -> 0;
        _ when X rem 2 == 0 -> X + fib_even_sum_rec(Y, X + Y, MaxFibNum);
        _ -> fib_even_sum_rec(Y, X + Y, MaxFibNum)
    end.
```

2. Хвостовая рекурсия

Используем хвостовую рекурсию для подсчета чисел Фибоначчи и нахождении суммы всех четных чисел Фибоначчи.
В данном случае хвостовая рекурсия выражается в том, что в каждой функции рекурсивный вызов является последним
и все необходимые данные для дальнейших вычислений сразу передаются в аргументы нового вызова этой же функции

```erlang
fib_even_sum(X, Y, MaxFibNum) when X >= 0, Y >= 1, MaxFibNum > 0 ->
  fib_even_sum(X, Y, 0, MaxFibNum).
fib_even_sum(X, _Y, Acc, MaxFibNum) when X > MaxFibNum ->
  Acc;
fib_even_sum(X, Y, Acc, MaxFibNum) when X rem 2 == 0 ->
  fib_even_sum(Y, X + Y, Acc + X, MaxFibNum);
fib_even_sum(X, Y, Acc, MaxFibNum) ->
  fib_even_sum(Y, X + Y, Acc, MaxFibNum).
```

3. Модульная реализация (filter, fold) не из BIF

В этой реализации можно явно разделить решение задачи на 3 част:
- генерация последовательности чисел Фибоначчи
- фильтрация последовательности (удаление всех нечетных элементов из списка)
- сворачивание списка (суммирование всех элементов списка)

В этой реализации функции filter/fold были использованы **не** из BIF, чтобы лучше понять
как они работают
```erlang
generate_fib(X, _Y, MaxNum) when X >= MaxNum -> [];
generate_fib(X, Y, MaxNum) -> [X | generate_fib(Y, X + Y, MaxNum)].

filter_seq(_, [], Acc) ->
  Acc;
filter_seq(Pred, [H | T], Acc) ->
  case Pred(H) of
    true -> filter_seq(Pred, T, [H | Acc]);
    false -> filter_seq(Pred, T, Acc)
  end.

fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(Start, H), T).

sum(X, Y) -> X + Y.

is_even(X) -> X rem 2 == 0.

start(X, Y, MaxFibNum) ->
  FibNums = generate_fib(X, Y, MaxFibNum),
  EvenFibList = filter_seq(fun is_even/1, FibNums, []),
  fold(fun sum/2, 0, EvenFibList).
```

4. Модульная реализация с использование filter, fold из BIF + анонимные функции

Тут можно заметить, что в целом решение сильно уменьшается и при желании его можно
написать в одну строчку. К тому тут очень хорошо вписываются анонимные функции, хотя
читаемость кода немного снижается

```erlang
-define(MAX_FIB_NUM, 4000000).

start(K, Z, MaxFibNum) ->
  FibList = task2_module:generate_fib(K, Z, MaxFibNum),
  EvenFibList = lists:filter(fun(X) -> X rem 2 == 0 end, FibList),
  lists:foldl(fun(X, Y) -> X + Y end, 0, EvenFibList).
```

5. Генерация последовательности при помощи встроенного map

Генерация самих чисел Фибоначчи возьмем из модуля `task2_module`. Поиск четных чисел из списка
реализован при помощи map, а удаление ненужных элементов делаем при помощи функции filter.
`foldl` -- проходит слева направо и сворачивает наш список (суммирует все элементы списка)
```erlang
generate_fib_list_map(Z, K, MaxFibNum) ->
  FibList = task2_module:generate_fib(Z, K, MaxFibNum),
  MappedList = lists:map(
    fun
      (X) when X rem 2 == 0 -> X;
      (_) -> false
    end,
    FibList
  ),
  lists:filter(fun(X) -> X /= false end, MappedList).

start(Z, K, MaxFibNum) ->
  lists:foldl(fun(X, Y) -> X + Y end, 0, generate_fib_list_map(Z, K, MaxFibNum)).
```

6. Генерация последовательности с кастомным map (map_until)

Встроенная функция map работает так, что мы проходимся по списку и применяем к каждому элементу списка
переданную функцию, однако мы не можем остановить выполнение функции map, когда определенное условие
не соблюдается, поэтому для генерации последовательности чисел Фибоначчи можно написать свою реализацию map
```erlang
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
```


## Задача 29

### Описание задачи
Рассмотрим все целочисленные комбинации $a^b$ для $2 <= a <= 5$ и $2 <= b <= 5$. Если посчитать все значения
и расположить их в числовом порядке от меньшего к большему, мы получим следующую последовательность различных
термов: $4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125$

Сколько различных чисел содержится в последовательности сгенерированной $a^b$, где $2 <= a <= 100$ и $2 <= b <= 100$

### Решения задачи

1. Рекурсия

Рекурсивно создаем список, который содержит вычисленное выражение $a^b$ для $a, b$ в указанных диапазонах.
`iterate_base` перебирает все основания, а `generate_powers` создает список всех степеней, где $a$ -- константа, а $b$ --
перебирается в указанном диапазоне `[Power, MaxPower]`.

```erlang
pow(_, 0) -> 1;
pow(X, Y) when Y >= 1 -> X * pow(X, Y - 1).

iterate_base(Base, MaxBase, _Power, _MaxPower) when Base > MaxBase -> [];
iterate_base(Base, MaxBase, Power, MaxPower) ->
    generate_powers(Base, Power, MaxPower) ++
        iterate_base(Base + 1, MaxBase, Power, MaxPower).

generate_powers(_, Power, MaxPower) when Power > MaxPower -> [];
generate_powers(Base, Power, MaxPower) ->
    [pow(Base, Power) | generate_powers(Base, Power + 1, MaxPower)].
```

2. Хвостовая рекурсия

Добавляем аккумулятор в виде списка в функции. Таким образом, на каждом 
рекурсивном вызове в параметры функции передаются все необходимые 
значения для дальнейших вычислений
```erlang
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
```

3. Модульная реализация (fold, filter)

Генерируем последовательность всех возможных пар `{Base, Power}` с помощью 
генератора. Создаем функцию `kepp_unique`, которая будет отсеивать
все вычисленные дубликаты, используем ее для фильтрации. С помощью
`fold` считаем длину отфильтрованного списка. 
```erlang
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
```

4. Отображение (map)

Отображение используем для вычисления значения вида `{Base, Power}`
```erlang
generate_list(Left, Right) ->
    [{X, Y} || X <- lists:seq(Left, Right), Y <- lists:seq(Left, Right)].

generate_powers(Left, Right) ->
    lists:map(fun({X, Y}) -> round(math:pow(X, Y)) end, generate_list(Left, Right)).

find_length_of_unique_powers(Left, Right) ->
    length(lists:usort(generate_powers(Left, Right))).
```

5. Простое решение с генератором списка

Используем силу генераторов в Эрланге. 
```erlang
find_distinct(Left, Right) ->
    length(
        lists:usort([math:pow(X, Y) || X <- lists:seq(Left, Right), Y <- lists:seq(Left, Right)])
    ).
```
## Вывод
В результате выполнения лабораторной работе я познакомился с синтаксисом Erlang и применил на практике некоторые
его концепции, которые также присуще большинству функциональным языкам программирования. Например, рекурсия, с помощью
которой можно сделать циклы, и хвостовая рекурсия, которая может уменьшить потребление памяти программой и улучшить 
читаемость кода. Также фильтрация, свертка и отображения, которые также базируются на рекурсии, и оказываются безумно удобными для генерации
необходимых последовательностей особенно с использованием анонимных функций. 
Генераторы списков, которые основываются на математическом записи множеств, также
сильно упрощают работу. И конечно же ключевой механизм в Erlang -- pattern matching, позволяющий строить
функции, которые автоматически выбирают правильную ветвь для обработки различных данных, игнорировать какие-то значения
или без особых трудностей извлекать данные из структур данных, обеспечивая при этом лаконичность и выразительность кода. 

Сравнивая реализации задач Эйлера на Erlang и Java, можно сказать, что решение задач на Erlang выглядит более 
лаконичным и компактным за счет декларативного стиля, удобной работы с рекурсией и мощного pattern matching, 
который позволяет минимизировать количество явных условий и сложных конструкций в коде.

