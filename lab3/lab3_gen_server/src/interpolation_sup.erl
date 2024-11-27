-module(interpolation_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/1]).

-export([init/1]).

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
init([Methods]) ->
    Children = lists:map(
        fun(Method) ->
            case Method of
                linear ->
                    {linear_server, {linear_server, start_link, []}, permanent, 5000,
                        worker, [linear_server]};
                lagrange ->
                    {lagrange_server, {lagrange_server, start_link, []}, permanent, 5000,
                        worker, [lagrange_server]};
                UMethod ->
                    io:fwrite("Uknown method: ~p, skipped~n", [UMethod]),
                    undefined
            end
        end,
        Methods
    ),
    FilteredChildren = lists:filter(fun(X) -> X =/= undefined end, Children),

    {ok, {{one_for_one, 5, 10}, FilteredChildren}}.

%% internal functions
