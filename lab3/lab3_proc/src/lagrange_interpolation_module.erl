-module(lagrange_interpolation_module).

-export([
    start_lagrange/3,
    lagrange_loop/4,
    lagrange_multiplier/3,
    lagrange_polynomial/2,
    evaluate_lagrange/2
]).

start_lagrange(From, Step, Window) ->
    spawn(fun() -> lagrange_loop([], Step, Window, From) end).

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

lagrange_loop(Points, Step, Window, From) ->
    % io:format("Current Points: ~p~n", [Points]),
    receive
        {data, Data} ->
            UpdatedPoints =
                case length(Points) of
                    Window ->
                        tl(Points) ++ [Data];
                    _ ->
                        Points ++ [Data]
                end,

            case length(UpdatedPoints) of
                Window ->
                    Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
                    InterpolatedValues = evaluate_lagrange(Step, Sorted),
                    From ! {ok, lagrange, InterpolatedValues};
                _ ->
                    lagrange_loop(UpdatedPoints, Step, Window, From)
            end,

            lagrange_loop(UpdatedPoints, Step, Window, From);
        _ ->
            lagrange_loop(Points, Step, Window, From)
    end.
