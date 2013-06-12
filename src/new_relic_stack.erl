-module(new_relic_stack).

-export([maybe_report_total/1,
         apply/2,
         start/1, stop/0, done/0, init/1,
         set_url/1,
         background_init/0,
         erase_state/0, get_state/0, init_from_state/1]).

-record(ep,
        {name :: term(), %% is sent to newrelic via statman.
         times = [] :: [{StartTime::tuple(), EndTime::tuple()|undefined}]}).

-record(runtime_stat,
        {done = [] :: [#ep{}],
         url :: binary(),
         queue = [] :: [#ep{}]
        }).

-define(NEW_RELIC_STACK, new_relic_stack).

%% ---------------------------------------------------------------------------
%% Normal usecase API

%% Inits a stack
init(Url) ->
    set_state(#runtime_stat{url = Url}).

%% Removes a stack and reports it to newrelic (via statman).
done() ->
    done(erase_state()).

%% this is a shorthand for ... what you see in the function.
%% use ?NR(... work ...) if you want to measure time spent in a modulel
%% and don't want to clutter up your code to much.
apply(Name, Fun) ->
    try
        start(Name),
        Fun()
    after
        stop()
    end.

%% On a already existing stack:
%% * Pauses the currently running timer
%% * Starts a new timer
%% * Puts it on the stack
start(Name) ->
    case get_state() of
        undefined -> ok;
        State -> set_state(start(State, fix_name(Name)))
    end,
    ok.

%% Stop works in pair with start/1.
stop() ->
    case get_state() of
        undefined -> ok;
        State -> set_state(stop(State))
    end,
    ok.

%% ---------------------------------------------------------------------------
%% Special usecase API

%% If you want to rewrite the url on the fly somewhere.
set_url(Url) ->
    case get_state() of
        undefined -> ok;
        RunTimeStat -> set_state(RunTimeStat#runtime_stat{url = Url})
    end.

%% Run this function on the result of done/0 if you don't want to roll your
%% own time measurement.
maybe_report_total(undefined) -> ok;
maybe_report_total({Url, Time}) ->
    statman_histogram:record_value({Url, total}, statman_histogram:bin(Time)).

%% Use this instead of init/1 if you want the work in
%% this process to be reported as background work.
background_init() ->
    set_state(#runtime_stat{}).

%% Use this in conjuntion with erase_state/0 or get_state/0
%% if you have serial work that is done in several processes
%% (eg. some part of call is done in a gen_server/gen_fsm).
init_from_state(undefined) -> ok;
init_from_state(#runtime_stat{url = Url}) ->
    set_state(#runtime_stat{url = Url}).

%% ---------------------------------------------------------------------------
%% API HELPERS

start(State, Name) ->
    State1 = maybe_stop_previous(State),
    add_new(Name, State1).

stop(State) ->
    State1 = finish(State),
    maybe_start_previous(State1).

done(undefined) -> undefined;
done(#runtime_stat{done = Done, url = Url, queue = []}) ->
    F = fun(EP) -> report(EP) end,
    lists:foreach(F, Done),
    {Url, total_time(Done)}.

total_time(Done) ->
    lists:sum(lists:map(fun ep_time/1, Done)).

report(EP) ->
    statman_histogram:record_value(
      EP#ep.name, statman_histogram:bin(ep_time(EP))).

ep_time(Ep) ->
    lists:foldl(
      fun({Start, Stop}, Acc) -> Acc + timer:now_diff(Stop, Start) end,
      0, Ep#ep.times).

fix_name(Name) ->
    case url() of
        undefined -> Name;
        Url -> {Url, Name}
    end.

url() ->
    case get_state() of
        #runtime_stat{url = Url} -> Url;
        undefined -> undefined
    end.

%% ---------------------------------------------------------------------------
%% State process dictionary storing

erase_state() ->
    erase(?NEW_RELIC_STACK).

set_state(State) ->
    put(?NEW_RELIC_STACK, State).

get_state() ->
    get(?NEW_RELIC_STACK).

%% ---------------------------------------------------------------------------
%% State handling

finish(State) ->
    [#ep{times = [{Start, undefined}|Times]}=Ep|Eps] =
        State#runtime_stat.queue,
    State#runtime_stat{
      queue = Eps,
      done = [Ep#ep{times = [{Start, t()}|Times]}|State#runtime_stat.done]}.

maybe_start_previous(State) ->
    case State#runtime_stat.queue of
        [] -> State;
        [#ep{times = Times}=Ep|Eps] ->
            NewEp = Ep#ep{times = [{t(), undefined}|Times]},
            State#runtime_stat{queue = [NewEp|Eps]}
    end.

maybe_stop_previous(State) ->
    case State#runtime_stat.queue of
        [] -> State;
        [#ep{times = [{Start, undefined}|Times]}=Ep|Eps] ->
            NewEp = Ep#ep{times = [{Start, t()}|Times]},
            State#runtime_stat{queue = [NewEp|Eps]}
    end.

add_new(Name, State) ->
    Ep = #ep{name = Name, times = [{t(), undefined}]},
    State#runtime_stat{queue = [Ep|State#runtime_stat.queue]}.

t() -> os:timestamp().
