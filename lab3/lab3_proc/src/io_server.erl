-module(io_server).

-export([start_input/2, input_loop/2, start_output/0, output_loop/0, round_to/2]).

start_input(PPid, Pid) ->
    input_loop(PPid, Pid).

input_loop(ParentPid, Pid) ->
    receive
        {exit, ok} -> exit(ok)
    after 100 ->
        io:fwrite("~nEnter dots: "),
        Input = string:trim(io:get_line("")),
        case Input of
            "exit" ->
                ParentPid ! {exit, ok};
            _ ->
                ParsedInput = parse_point(Input),
                Filtered = lists:filter(fun(X) -> is_number(X) end, ParsedInput),
                case length(Filtered) of
                    2 ->
                        interpid ! {interpolate, ParsedInput},
                        input_loop(ParentPid, Pid);
                    _ ->
                        io:fwrite("Usage: X Y~n")
                end,
                input_loop(ParentPid, Pid)
        end
    end.

parse_point(Line) ->
    [
        case string:to_float(Num) of
            {error, _} ->
                case string:to_integer(Num) of
                    {error, _} ->
                        io:fwrite("~p - is not a number. Invalid input~n", [Num]);
                    Integer ->
                        element(1, Integer)
                end;
            Float ->
                element(1, Float)
        end
     || Num <- string:tokens(Line, " ")
    ].

start_output() ->
    register(opid, self()),
    output_loop().

output_loop() ->
    receive
        {ok, linear, Result} ->
            io:fwrite("Linear interpolation result~n"),
            print_result(Result),
            output_loop();
        {error, Method, Msg} ->
            error_logger:error_msg(
                "\e[31mError in method ~p -- ~p\e[0m~n",
                [Method, Msg]
            );
        {exit, ok} ->
            exit(ok);
        {ok, lagrange, Result} ->
            io:fwrite("~nLagrange interpolation result~n"),
            print_result(Result),
            output_loop()
    end.

print_result([GeneratedDots, InterpolatedValues]) ->
    io:fwrite("Generated Dots:~n"),
    lists:foreach(
        fun(Dot) -> io:fwrite("~8.3f ", [float(Dot)]) end,
        GeneratedDots
    ),
    io:fwrite("~nInterpolated Values:~n"),
    lists:foreach(
        fun(Value) -> io:fwrite("~8.3f ", [float(round_to(Value, 3))]) end,
        InterpolatedValues
    ),
    io:fwrite("~n").

round_to(Number, Digits) ->
    Factor = math:pow(10, Digits),
    erlang:round(Number * Factor) / Factor.
