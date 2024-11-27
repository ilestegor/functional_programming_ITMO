%%%-------------------------------------------------------------------
%%% @author ilestegor
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2024 1:50 PM
%%%-------------------------------------------------------------------
%%%
-module(main).

%% API
-export([start/0, parse_config/0, strings_to_atoms/1]).

start() ->
    process_flag(trap_exit, true),
    io:fwrite("Starting supervisor...~n"),

    {Freq, Window, Methods} = main:parse_config(),
    io:fwrite("Booted with -- Freq ~p, Window ~p, Methods ~p~n", [Freq, Window, Methods]),
    InterPid = spawn_link(interpolation_module, start, [{Freq, Window, Methods}]),
    InputPid = spawn_link(io_server, start_input, [self(), InterPid]),
    OutputPid = spawn_link(io_server, start_output, []),
    supervisor_loop([
        {interpolation, InterPid}, {io_input, InputPid}, {io_output, OutputPid}
    ]).

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
terminate_children([]) ->
    ok;
terminate_children([{_Tag, Pid} | Rest]) ->
    Pid ! {exit, ok},
    terminate_children(Rest).

wait_for_children(0) ->
    io:fwrite("All child processes have terminated.~n"),
    io:fwrite("Application stopped.~n"),
    exit(self(), ok);
wait_for_children(N) ->
    receive
        {'EXIT', _From, _Reason} ->
            wait_for_children(N - 1)
    end.
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

% Парсинг CLI аргументов
parse_config() ->
    Freq = get_arg_value(freq, 1),
    Window = get_arg_value(w, 3),
    Methods = strings_to_atoms(get_multi_arg(methods, [])),
    {Freq, Window, Methods}.

get_arg_value(Key, Default) ->
    case init:get_argument(Key) of
        {ok, [[Value]]} -> parse_number(Value);
        {ok, []} -> Default;
        _ -> Default
    end.

get_multi_arg(Key, Default) ->
    case init:get_argument(Key) of
        {ok, Values} -> lists:append(Values);
        _ -> Default
    end.

strings_to_atoms(StrList) ->
    lists:map(fun erlang:list_to_atom/1, StrList).

parse_number(Num) ->
    case string:to_float(Num) of
        {error, _} ->
            case string:to_integer(Num) of
                {error, _} -> io:fwrite("Parse error");
                Integer -> element(1, Integer)
            end;
        Float ->
            element(1, Float)
    end.
