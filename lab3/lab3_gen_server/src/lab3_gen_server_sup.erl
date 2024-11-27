%%%-------------------------------------------------------------------
%% @doc lab3_gen_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lab3_gen_server_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([{Freq, Window, Methods}]) ->
    Children = [
        % Unique ID
        {output_server,
            % Start function
            {output_server, start_link, []},
            % Restart strategy
            permanent,
            % Shutdown timeout (ms)
            5000,
            % Process type
            worker,
            % Modules list
            [output_server]},

        % Unique ID
        {interpolation_sup,
            % Start function
            {interpolation_sup, start_link, [Methods]},
            % Restart strategy
            permanent,
            % Shutdown timeout (ms)
            5000,
            % Process type
            supervisor,
            % Modules list
            [interpolation_sup]},

        % Unique ID
        {interpolation_server,
            % Start function
            {interpolation_server, start_link, [{Freq, Window}]},
            % Restart strategy
            permanent,
            % Shutdown timeout (ms)
            5000,
            % Process type
            worker,
            % Modules list
            [interpolation_server]},

        % Unique ID
        {input_server,
            % Start function
            {input_server, start_link, []},
            % Restart strategy
            permanent,
            % Shutdown timeout (ms)
            5000,
            % Process type
            worker,
            % Modules list
            [input_server]}
    ],

    {ok, {{one_for_one, 5, 10}, Children}}.

%% internal functions
