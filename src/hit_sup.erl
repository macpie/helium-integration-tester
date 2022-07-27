%%%-------------------------------------------------------------------
%% @doc hit top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hit_sup).

-behaviour(supervisor).

-include("hit.hrl").

-export([start_link/0]).

-export([init/1]).

-define(FLAGS, #{
    strategy => rest_for_one,
    intensity => 1,
    period => 5
}).

-define(WORKER(I, Args), #{
    id => I,
    start => {I, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [I]
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    lager:info("init"),

    {ok, _} = application:ensure_all_started(lager),

    _ = prometheus_gauge:declare([
        {name, ?METRIC_REQ_GAUGE},
        {help, "Helium Integration Tester Request Gauge"},
        {labels, [type, device_id, hotspot]}
    ]),

    ElliOpts = [{callback, hit_worker}, {port, 80}],
    ChildSpecs = [?WORKER(elli, [ElliOpts])],
    {ok, {?FLAGS, ChildSpecs}}.
