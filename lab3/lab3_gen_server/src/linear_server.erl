-module(linear_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([linear_interpolation/2]).

%% Starts the output server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Callbacks
init([]) ->
    {ok, []}.

handle_cast({linear, Input, Freq}, State) ->
    %% Add the new point to the state
    NewState =
        case length(State) of
            2 -> tl(State) ++ [Input];
            _ -> State ++ [Input]
        end,

    %% Perform interpolation if there are exactly two points
    NewState =
        case NewState of
            [P1, P2] ->
                Sorted = lists:sort(fun([A, _], [B, _]) -> A =< B end, [P1, P2]),
                Result = linear_interpolation(Freq, Sorted),
                case Result of
                    {ok, linear, Res} ->
                        gen_server:cast(interpolation_server, {ok, linear, Res}),
                        NewState;
                    {error, linear, Msg} ->
                        gen_server:cast(interpolation_server, {error, linear, Msg}),
                        NewState
                end;
            _ ->
                NewState
        end,

    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

% Logic

linear_interpolation(Step, [[X1, Y1], [X2, Y2]]) ->
    case X1 =:= X2 of
        true ->
            {error, linear, "X values are equal"};
        false ->
            K = (Y2 - Y1) / (X2 - X1),
            B = Y1 - K * X1,
            GenerateValues = interpolation_server:points_generator(Step, X1, X2),
            Res = [GenerateValues, lists:map(fun(X) -> K * X + B end, GenerateValues)],
            {ok, linear, Res}
    end.
