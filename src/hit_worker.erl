-module(hit_worker).

-behaviour(elli_handler).

-include("hit.hrl").

-export([
    handle/2,
    handle_event/3
]).

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET', [<<"metrics">>], _Req) ->
    {ok, [], prometheus_text_format:format()};
handle('POST', [<<"hit">>], Req) ->
    Body = jsx:decode(elli_request:body(Req), [return_maps, {labels, atom}]),
    Type = maps:get(type, Body, undefined),
    DeviceID = maps:get(id, Body, undefined),
    FCnt = maps:get(fcnt, Body, -1),
    lager:md([{device_id, DeviceID}]),
    lager:info("got req for ~p @ fcnt ~p", [Type, FCnt]),
    Hotspots = maps:get(hotspots, Body, []),
    lists:foreach(
        fun(HotspotData) ->
            HotspotName = maps:get(name, HotspotData, undefined),
            RSSI = maps:get(rssi, HotspotData, undefined),
            SNR = maps:get(snr, HotspotData, undefined),
            HoldTime = maps:get(hold_time, HotspotData, undefined),
            prometheus_counter:inc(?METRIC_DEVICE_PACKETS, [DeviceID, HotspotName]),
            prometheus_gauge:set(?METRIC_DEVICE_PACKETS_STATS, [DeviceID, HotspotName, rssi], RSSI),
            prometheus_gauge:set(?METRIC_DEVICE_PACKETS_STATS, [DeviceID, HotspotName, snr], SNR),
            prometheus_gauge:set(
                ?METRIC_DEVICE_PACKETS_STATS, [DeviceID, HotspotName, hold_time], HoldTime
            ),
            lager:info("seen by ~p rssi ~p snr ~p", [HotspotName, RSSI, SNR])
        end,
        Hotspots
    ),
    prometheus_gauge:set(?METRIC_HOTSPOTS_PER_DEVICE, [DeviceID], erlang:length(Hotspots)),
    {ok, [], <<>>};
handle(_Method, _Path, _Req) ->
    lager:debug("got unknown req ~p on ~p", [_Method, _Path]),
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
