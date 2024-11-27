-module(interpolation_server).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/0]).
%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([points_generator/3]).

%% Starts the output server
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Callbacks
init([{Freq, Window}]) ->
    {ok, {Freq, Window}}.

handle_cast({interpolate, Input}, State) ->
    gen_server:cast(linear_server, {linear, Input, erlang:element(1, State)}),
    gen_server:cast(
        lagrange_server,
        {lagrange, Input, erlang:element(2, State), erlang:element(1, State)}
    ),
    {noreply, State};
handle_cast({ok, linear, Result}, State) ->
    gen_server:cast(output_server, {ok, linear, Result}),
    {noreply, State};
handle_cast({ok, lagrange, Result}, State) ->
    gen_server:cast(output_server, {ok, lagrange, Result}),
    {noreply, State};
handle_cast({error, Method, Msg}, State) ->
    gen_server:cast(output_server, {error, Method, Msg}),
    {noreply, State};
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

points_generator(Step, X, Y) when Step > 0, X - Step =< Y ->
    [X | points_generator(Step, X + Step, Y)];
points_generator(_, _, _) ->
    [].
