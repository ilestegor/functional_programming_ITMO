-module(input_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, parse_point/1]).
%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Starts the input server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Callbacks
init([]) ->
    input_loop().

input_loop() ->
    %% Get user input
    timer:sleep(100),
    io:fwrite("~nEnter dot: "),
    Input = string:trim(io:get_line("")),
    Filtered = lists:filter(fun(X) -> is_number(X) end, parse_point(Input)),
    case length(Filtered) of
        2 ->
            gen_server:cast(interpolation_server, {interpolate, Filtered}),
            input_loop();
        _ ->
            io:fwrite("Usage: X Y~n"),
            input_loop()
    end.
handle_info(_, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
