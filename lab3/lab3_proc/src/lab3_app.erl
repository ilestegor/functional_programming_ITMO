%%%-------------------------------------------------------------------
%% @doc lab3 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab3_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab3_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
