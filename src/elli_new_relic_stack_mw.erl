-module(elli_new_relic_stack_mw).

-export([preprocess/2, handle_event/3]).

-define(END_EVENTS, [request_complete, chunk_complete]).

preprocess(Req, Args) ->
    new_relic_stack:init(url(Req, Args)),
    Req.

handle_event(Event, EventArgs, Args) ->
    case lists:member(Event, ?END_EVENTS) of
        true  -> {Url, _StackTotalTime} = new_relic_stack:done(),
                 log_total(EventArgs, Url);
        false -> ok
    end.

log_total([Req, _ResponseCode, _ResponseHeaders, _, Timings], Url) ->
    RequestTime = timings_diff(Timings, user_start, user_end),
    statman_histogram:record_value(
      {Url, total}, statman_histogram:bin(RequestTime)).

timings_diff(Timings, Start, End) ->
    TimeF = fun(Key) -> element(2, lists:keyfind(Key, 1, Timings)) end,
    timer:now_diff(TimeF(End), TimeF(Start)).

url(Req, Args) ->
    (proplists:get_value(url_rewrite, Args))(Req).
