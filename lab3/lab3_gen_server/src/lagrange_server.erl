-module(lagrange_server).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([lagrange_multiplier/3, lagrange_polynomial/2, evaluate_lagrange/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, []}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({lagrange, Input, Window, Freq}, State) ->
    UpdatedPoints =
        case length(State) of
            Window ->
                tl(State) ++ [Input];
            _ ->
                State ++ [Input]
        end,
    case length(UpdatedPoints) of
        Window ->
            Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, UpdatedPoints),
            InterpolatedValues = evaluate_lagrange(Freq, Sorted),
            gen_server:cast(interpolation_server, {ok, lagrange, InterpolatedValues});
        _ ->
            ok
    end,
    {noreply, UpdatedPoints}.

handle_info(_, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Lagrange multiplier
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

% Lagrange polynomial
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
    GeneratedDots = interpolation_server:points_generator(Step, X1, X2),
    [GeneratedDots, [lagrange_polynomial(X, Points) || X <- GeneratedDots]].
