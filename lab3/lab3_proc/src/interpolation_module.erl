-module(interpolation_module).

-export([start/1, init/1, interpolation_loop/2, points_generator/3]).

start(Config) ->
    register(interpid, self()),
    init(Config).

init(Config) ->
    Methods = element(3, Config),

    Procs = lists:map(
        fun(X) ->
            case X of
                linear ->
                    Pid = linear_interpolation:start_linear(element(1, Config), self()),
                    Ref = erlang:monitor(process, Pid),
                    {Pid, Ref, linear};
                lagrange ->
                    Pid = lagrange_interpolation_module:start_lagrange(
                        self(), element(1, Config), element(2, Config)
                    ),
                    Ref = erlang:monitor(process, Pid),
                    {Pid, Ref, lagrange};
                Method ->
                    io:fwrite("Unknown method: ~p , skipped~n", [Method]),
                    undefined
            end
        end,
        Methods
    ),

    ValidProcs = lists:filter(fun(X) -> X =/= undefined end, Procs),
    interpolation_loop(ValidProcs, Config).

interpolation_loop(Procs, Config) ->
    receive
        {interpolate, Data} ->
            [Pid ! {data, Data} || {Pid, _, _} <- Procs],
            interpolation_loop(Procs, Config);
        {ok, Method, Result} ->
            opid ! {ok, Method, Result},
            interpolation_loop(Procs, Config);
        {error, Method} ->
            opid ! {error, Method, "Interpolation failed"},
            interpolation_loop(Procs, Config);
        {exit, ok} ->
            lists:foreach(
                fun({Pid, Ref, _Method}) ->
                    erlang:demonitor(Ref, [flush]),
                    exit(Pid, ok)
                end,
                Procs
            ),
            exit(ok);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            {OldProc, RestProcs} = lists:partition(
                fun({_, R, _}) -> R =:= Ref end, Procs
            ),
            io:fwrite("Proc is down, restarting: ~p~n", [Ref]),
            case OldProc of
                [{_, _, Method}] ->
                    erlang:demonitor(Ref, [flush]),
                    NewPid = restart_process(Method, Config),
                    NewRef = erlang:monitor(process, NewPid),
                    NewProcs = [{NewPid, NewRef, Method} | RestProcs],
                    interpolation_loop(NewProcs, Config);
                [] ->
                    interpolation_loop(Procs, Config)
            end
    end.

restart_process(Method, Config) ->
    case Method of
        linear ->
            linear_interpolation:start_linear(element(1, Config), self());
        lagrange ->
            lagrange_interpolation_module:start_lagrange(
                self(), element(1, Config), element(2, Config)
            )
    end.

points_generator(Step, X, Y) when Step > 0, X - Step =< Y ->
    [X | points_generator(Step, X + Step, Y)];
points_generator(_, _, _) ->
    [].
