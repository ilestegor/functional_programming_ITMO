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
-define(MAX_FIB_NUM, 4000000).

fib_even_sum_rec(X, Y) when X >= 0, Y >= 1, ?MAX_FIB_NUM >= 0 ->
  if
    X > ?MAX_FIB_NUM -> 0; X rem 2 == 0 -> X + fib_even_sum_rec(Y, X + Y);
    true -> fib_even_sum_rec(Y, X + Y)
  end.
fib_even_sum_rec() ->
  io:format("~s ~p ~s ~p", ["Sum of even-valued Fibonacci terms that do not exceed", ?MAX_FIB_NUM, " -- ", fib_even_sum_rec(0, 1)]).
```

2. Хвостовая рекурсия

Используем хвостовую рекурсию для подсчета чисел Фибоначчи и нахождении суммы всех четных чисел Фибоначчи.
В данном случае хвостовая рекурсия выражается в том, что в каждой функции рекурсивный вызов является последним
и все необходимые данные для дальнейших вычислений сразу передаются в аргументы нового вызова этой же функции

```erlang
-define(MAX_FIB_NUM, 4000000).

solve() when ?MAX_FIB_NUM >= 0 ->
  io:format("~s ~p ~s ~p", ["Sum of even-valued Fibonacci terms that do not exceed", ?MAX_FIB_NUM, " -- ",  fib_even_sum(0, 1)]).

fib_even_sum(X, Y) when X >= 0, Y >= 1 ->
  fib_even_sum(X, Y, 0).


fib_even_sum(X, _Y, Acc) when X > ?MAX_FIB_NUM ->
  Acc;
fib_even_sum(X, Y, Acc) when X rem 2 == 0 ->
  fib_even_sum(Y, X + Y, Acc + X);
fib_even_sum(X, Y, Acc)  ->
  fib_even_sum(Y, X + Y, Acc).
```

3. Модульная реализация (filter, fold) не из BIF

В этой реализации можно явно разделить решение задачи на 3 част:
- генерация последовательности чисел Фибоначчи
- фильтрация последовательности (удаление всех нечетных элементов из списка)
- сворачивание списка (суммирование всех элементов списка)

В этой реализации функции filter/fold были использованы **не** из BIF, чтобы лучше понять
как они работают
```erlang
-define(MAX_FIB_NUM, 4000000).

%%generate fib sequence
generate_fib(X, _Y, MaxNum) when X >= MaxNum -> [];
generate_fib(X, Y, MaxNum) -> [X | generate_fib(Y, X + Y, MaxNum)].

%%filter sequence
filter_seq(_, [], Acc) -> Acc;
filter_seq(Pred, [H|T], Acc) ->
  case Pred(H) of
    true -> filter_seq(Pred, T, [H|Acc]);
    false -> filter_seq(Pred, T, Acc)
  end.

%%fold
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(Start, H), T).

sum(X, Y) -> X + Y.

is_even(X) -> X rem 2 == 0.


start() ->
  FibNums = generate_fib(0, 1, ?MAX_FIB_NUM),
  EvenFibList = filter_seq(fun is_even/1, FibNums, []),
  Sum = fold(fun sum/2, 0, EvenFibList),
  io:format("~s ~p ~s ~p", ["Sum of even-valued Fibonacci terms that do not exceed", ?MAX_FIB_NUM, " -- ", Sum]).
```

4. Модульная реализация с использование filter, fold из BIF + анонимные функции

Тут можно заметить, что в целом решение сильно уменьшается и при желании его можно
написать в одну строчку. К тому тут очень хорошо вписываются анонимные функции, хотя
читаемость кода немного снижается

```erlang
-define(MAX_FIB_NUM, 4000000).

start() ->
  FibList = task2_module:generate_fib(0, 1, ?MAX_FIB_NUM),
  EvenFibList = lists:filter(fun(X) -> X rem 2 == 0 end, FibList),
  Sum = lists:foldl(fun(X, Y) -> X + Y end, 0, EvenFibList),
  io:format("~p", [Sum]).
```

5. Генерация последовательности при помощи встроенного map

Генерация самих чисел Фибоначчи возьмем из модуля `task2_module`. Поиск четных чисел из списка
реализован при помощи map, а удаление ненужных элементов делаем при помощи функции filter.
`foldl` -- проходит слева направо и сворачивает наш список (суммирует все элементы списка)
```erlang
-define(MAX_FIB_NUM, 4000000).

generate_fib_list_map() ->
  FibList = task2_module:generate_fib(0, 1, ?MAX_FIB_NUM),
  MappedList = lists:map(fun(X) when X rem 2 == 0 -> X; (_) -> false end, FibList),
  lists:filter(fun(X) -> X /= false end, MappedList).


start() -> io:format("~p", [lists:foldl(fun(X, Y) -> X + Y end, 0, generate_fib_list_map())]).
```

6. Генерация последовательности с кастомным map (map_until)

Встроенная функция map работает так, что мы проходимся по списку и применяем к каждому элементу списка
переданную функцию, однако мы не можем остановить выполнение функции map, когда определенное условие
не соблюдается, поэтому для генерации последовательности чисел Фибоначчи можно написать свою реализацию map
```erlang
-define(MAX_FIB_NUM, 4000000).

map_until(_, [], _) -> [];
map_until(Fun, [H | T], MaxValue) ->
  Result = Fun(H),
  if
    Result > MaxValue -> [];
    true -> [Result | map_until(Fun, T, MaxValue)]
  end.


fib(N) when N >= 0 ->
  fib(N, 1, 1).

fib(0, A, _) -> A;
fib(N, A, B) ->
  fib(N - 1, B, A + B).

generate_fib_list_map() ->
  List = lists:seq(0, 100),
  FibList = map_until(fun fib/1, List, ?MAX_FIB_NUM),
  lists:foldl(fun(X, Y) -> X + Y end, 0, lists:filter(fun(X) -> X rem 2 == 0 end, FibList)).

start() ->io:format("~p", [ generate_fib_list_map()]).
```