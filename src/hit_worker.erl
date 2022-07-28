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
        fun(Hotspot) ->
            Name = maps:get(name, Hotspot, undefined),
            prometheus_counter:inc(?METRIC_REQ_COUNTER, [DeviceID, Name]),
            lager:info("seen by ~p", [Name])
        end,
        Hotspots
    ),
    prometheus_gauge:set(?METRIC_REQ_GAUGE, [DeviceID], erlang:length(Hotspots)),
    {ok, [], <<>>};
handle(_Method, _Path, _Req) ->
    lager:debug("got unknown req ~p on ~p", [_Method, _Path]),
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
