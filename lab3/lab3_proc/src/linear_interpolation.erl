-module(linear_interpolation).

-export([start_linear/2, loop_linear/3, linear_interpolation/2]).

start_linear(Step, From) ->
    spawn(fun() -> loop_linear([], Step, From) end).

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

loop_linear(Points, Step, From) ->
    receive
        {data, Data} ->
            UpdatedPoints =
                case Points of
                    [_ | Rest] when length(Points) == 2 -> Rest ++ [Data];
                    _ -> Points ++ [Data]
                end,
            case length(UpdatedPoints) of
                2 ->
                    % [P1, P2] = UpdatedPoints,
                    Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
                    Result = linear_interpolation(Step, Sorted),
                    case Result of
                        {ok, linear, Res} -> From ! {ok, linear, Res};
                        _ -> From ! {error, linear}
                    end,
                    loop_linear(UpdatedPoints, Step, From);
                _ ->
                    loop_linear(UpdatedPoints, Step, From)
            end
    end.
