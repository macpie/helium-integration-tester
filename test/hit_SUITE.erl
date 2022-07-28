-module(hit_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("hit.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([basic_test/1]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [basic_test].

%%--------------------------------------------------------------------
%% TEST CASE SETUP
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(lager),
    _ = application:ensure_all_started(hit),
    Config.

%%--------------------------------------------------------------------
%% TEST CASE TEARDOWN
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    _ = application:stop(hit),
    _ = application:stop(hackney),
    _ = application:stop(lager),
    Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
basic_test(_Config) ->
    Data = load_json_file(),
    DeviceID = maps:get(id, Data),
    hackney:post(
        <<"http://127.0.0.1:80/hit">>,
        [],
        jsx:encode(Data),
        [with_body]
    ),
    lists:foreach(
        fun(Hotspot) ->
            Name = maps:get(name, Hotspot, undefined),
            ?assertEqual(1, prometheus_counter:value(?METRIC_REQ_COUNTER, [DeviceID, Name]))
        end,
        maps:get(hotspots, Data, [])
    ),
    Hotspots = maps:get(hotspots, Data, []),
    ?assertEqual(erlang:length(Hotspots), prometheus_gauge:value(?METRIC_REQ_GAUGE, [DeviceID])),
    ok.

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------

-spec load_json_file() -> map().
load_json_file() ->
    {ok, Bin} = file:read_file(code:lib_dir(hit) ++ "/test/data.json"),
    jsx:decode(Bin, [return_maps, {labels, atom}]).
