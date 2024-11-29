# Lab3

- Выполнил: Глотов Егор Дмитриевич
- Группа: P3332
- ИСУ: 368030

## Задание

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

  - какие алгоритмы использовать (в том числе два сразу);
  - частота дискретизации результирующих данных;
  - и т.п.;

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

В рамках 3 лабораторной было реализовано два варината приложения:

- с использованием стандартных процессов: [lab3_proc](https://github.com/ilestegor/functional_programming_ITMO/tree/main/lab3/lab3_proc)
- с использованием OTP [lab3_gen_server](https://github.com/ilestegor/functional_programming_ITMO/tree/main/lab3/lab3_gen_server)

## Lab3, spawn/receive

### Организация работы приложения

<img src="img/Screenshot 2024-11-29 at 10.54.22 AM.png" alt="drawing" width="600"/>

### Дерево процессов

<img src="img/Screenshot 2024-11-29 at 1.26.38 AM.png" alt="drawing" width="600"/>

- Точка входа в программу: [lab3_proc/src/main.erl](lab3_proc/src/main.erl)
- Работа с вводов/выводом: [lab3_proc/src/io_server.erl](lab3_proc/src/io_server.erl)
- Менеджер алгоритмов интерполяции [lab3_proc/src/interpolation_module.erl](lab3_proc/src/interpolation_module.erl)
- Линейная интерполяция [lab3_proc/src/linear_interpolation.erl](lab3_proc/src/linear_interpolation.erl)
- Метод Лагранжа [lab3_proc/src/lagrange_interpolation_module.erl](lab3_proc/src/lagrange_interpolation_module.erl)

### Ключевые элементы реализации

Линейная интерполяция:

```erlang
linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
    case X1 =:= X2 of
        true ->
            {error, linear};
        false ->
            K = (Y2 - Y1) / (X2 - X1),
            B = Y1 - K * X1,
            GenerateValues = interpolation_module:points_generator(Step, X1, X2),
            Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
            {ok, linear, Res}
    end.
```

Интерполяция методом Лагранжа

```erlang
lagrange_multiplier(X, Xi, Points) ->
    lists:foldl(
        fun
            ([Xj, _], Acc) when Xj =/= Xi ->
                Acc * (X - Xj) / (Xi - Xj);
            (_, Acc) ->
                Acc
        end,
        1,
        Points
    ).
lagrange_polynomial(X, Points) ->
    lists:foldl(
        fun([Xi, Yi], Acc) ->
            Acc + Yi * lagrange_multiplier(X, Xi, Points)
        end,
        0,
        Points
    ).
evaluate_lagrange(Step, Points) ->
    [X1, _] = hd(Points),
    [X2, _] = lists:last(Points),
    GeneratedDots = interpolation_module:points_generator(Step, X1, X2),
    [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].
```

Попытка создания супервизора, который перезапускает упавшие процессы

```erlang
supervisor_loop(ChildPids) ->
    receive
        {'EXIT', From, Reason} ->
            case Reason of
                ok ->
                    ok;
                _ ->
                    io:fwrite(
                        "Child process ~p exited with reason: ~p~n",
                        [From, Reason]
                    ),

                    RestartedChildPids = restart_child(From, ChildPids),

                    supervisor_loop(RestartedChildPids)
            end;
        {exit, Reason} ->
            io:fwrite("Shutdown requested: ~p~n", [Reason]),
            terminate_children(ChildPids),
            wait_for_children(length(ChildPids));
        _Other ->
            supervisor_loop(ChildPids)
    end.
```

Функция заново запускает упавший процесс при этом обновляя id процесса

```erlang
restart_child(From, ChildPids) ->
    case lists:keyfind(From, 2, ChildPids) of
        {Tag, _Pid} ->
            NewPid =
                case Tag of
                    interpolation ->
                        {Freq, Window, Methods} = main:parse_config(),
                        spawn_link(
                            interpolation_module,
                            start,
                            [{Freq, Window, Methods}]
                        );
                    io_input ->
                        {_, InterPid} = lists:keyfind(interpolation, 1, ChildPids),
                        spawn_link(io_server, start_input, [self(), InterPid]);
                    io_output ->
                        spawn_link(io_server, start_output, [])
                end,
            lists:keyreplace(Tag, 1, ChildPids, {Tag, NewPid});
        false ->
            io:fwrite("Unknown process ~p, not restarting.~n", [From]),
            ChildPids
    end.
```

Также для того, чтобы перезапсукать отдельно упавший процесс любого из методов интерполяции (сделать так, чтобы перезапускался только один процесс, а не все) были использованы мониторы

### Ввод/вывод программы

Запуск программы производится следующей командой
`erl -noshell -s main start -freq 1 -w 4 -methods linear lagrange -s init stop`

либо

можно воспользоваться скриптом `run.sh`, который запустит программы без надобности поиска скомпилированных файлов

Ключи:

- freq -- частота дискретизации
- w -- размер окна для метода Лагранжа
- methods -- методы, которые будут включены при запуске программы

Пример работы программы для ln(x)

```zsh
Starting supervisor...
Booted with -- Freq 0.5, Window 4, Methods [linear,lagrange]

Enter dots: 1 0

Enter dots: 2 0.693
Linear interpolation result
Generated Dots:
   1.000    1.500    2.000    2.500 
Interpolated Values:
   0.000    0.346    0.693    1.039 

Enter dots: 3 1.0986
Linear interpolation result
Generated Dots:
   2.000    2.500    3.000    3.500 
Interpolated Values:
   0.693    0.896    1.099    1.301 

Enter dots: 4 1.3863
Linear interpolation result
Generated Dots:
   3.000    3.500    4.000    4.500 
Interpolated Values:
   1.099    1.242    1.386    1.530 

Lagrange interpolation result
Generated Dots:
   1.000    1.500    2.000    2.500    3.000    3.500    4.000    4.500 
Interpolated Values:
   0.000    0.393    0.693    0.921    1.099    1.247    1.386    1.539 

Enter dots:
```

## Lab3, OTP

### Организация работы приложения

<img src="img/Screenshot 2024-11-29 at 11.06.08 AM.png" alt="drawing" width="600"/>

### Дерево процессов

<img src="img/Screenshot 2024-11-29 at 10.55.43 AM.png" alt="drawing" width="600"/>

- Точка входа в программу: [lab3_gen_server/src/lab3_gen_server_app.erl](lab3_gen_server/src/lab3_gen_server_app.erl)
- Работа с вводом: [lab3_gen_server/src/input_server.erl](lab3_gen_server/src/input_server.erl)
-Работы с выводом: [lab3_gen_server/src/output_server.erl](lab3_gen_server/src/output_server.erl)
- Менеджер интерполяции: [lab3_gen_server/src/interpolation_server.erl](lab3_gen_server/src/interpolation_server.erl)
- Линейная интерполяция: [lab3_gen_server/src/linear_server.erl](lab3_gen_server/src/linear_server.erl)
- Метод Лагранжа: [lab3_gen_server/src/lagrange_server.erl](lab3_gen_server/src/lagrange_server.erl)

### Ключевые элементы реализации

Код и его структура очень похожи на предыдущий вариант реализации приложения. Единственное, что поменялось -- это не такое болезненное создание супервизоров и то как процессы общаются между собой.

### Ввод/вывод программы

Запуск программы происходит аналогичным образом через команду `erl -noshell -s lab3_gen_server_app start -freq 1 -w 4 -methods linear lagrange -s init stop`

либо

через скрпт `run.sh`

Пример работы программы для ln(x)

```zsh
Booted with: freq - 0.5, window - 4, methods - [linear,lagrange]

Enter dot: 1 0

Enter dot: 2 0.693

Linear interpolation: 
Generated Dots:
   1.000    1.500    2.000    2.500 
Interpolated Values:
   0.000    0.346    0.693    1.039 

Enter dot: 3 1.0986

Linear interpolation: 
Generated Dots:
   2.000    2.500    3.000    3.500 
Interpolated Values:
   0.693    0.896    1.099    1.301 

Enter dot: 4 1.3863

Linear interpolation: 
Generated Dots:
   3.000    3.500    4.000    4.500 
Interpolated Values:
   1.099    1.242    1.386    1.530 

Lagrange interpolation: 
Generated Dots:
   1.000    1.500    2.000    2.500    3.000    3.500    4.000    4.500 
Interpolated Values:
   0.000    0.393    0.693    0.921    1.099    1.247    1.386    1.539 

Enter dot: 
```

## Вывод

В ходе выполнения лабораторной работы я познакомился с процессами, передачей сообщений между ними и особенностями работы с вводом/выводом в Erlang. Оба варианта приложения были реализованы без особых трудностей, однако использование обычных процессов оказалось чуть сложнее из-за необходимости вручную оперировать PID-ами для передачи сообщений. OTP значительно упрощает этот процесс, предоставляя удобную абстракцию, благодаря которой взаимодействие между процессами выглядит как вызов функций другого модуля, и о потоках даже не задумываешься.

Реализация супервизора на обычных процессах оказалась менее удобной, хотя полностью осуществимой. Использование же gen_server и супервизоров OTP делает эту задачу значительно проще и нагляднее. В целом, OTP — это мощная технология, которая позволяет быстро и эффективно разрабатывать многопроцессные приложения в Erlang.

Для стандартных задач OTP является идеальным решением, предлагая готовые шаблоны, где разработчику остается только добавить бизнес-логику. Однако использование обычных процессов предоставляет большую гибкость, что может быть полезно в случаях, когда задача нестандартна и плохо укладывается в шаблоны OTP.
