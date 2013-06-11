-module(elli_new_relic_stack).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").
-include("../include/new_relic_stack.hrl").

-export([preprocess/2,
         handle/2,
         postprocess/3,
         handle_event/3]).

-export([rewrite_elli_conf/1, rewrite_elli_conf/2]).

%% Adds itself inbetween elli_middelware and the real middleware.
%% and adds start/stop middleware as the first middelware
rewrite_elli_conf(Mods) ->
    DefaultUrlF = fun elli_request:raw_path/1,
    rewrite_elli_conf(Mods, DefaultUrlF).

rewrite_elli_conf(Mods, UrlRewriteF) ->
    Rewrite = lists:map(
                fun({Mod, ModArgs}) ->
                        {?MODULE, [{callback, Mod},
                                   {callback_args, ModArgs}]}
                end,
                Mods),
    io:format("rewrite ~p ~p", [Mods, Rewrite]),
    [{elli_new_relic_stack_mw, [{url_rewrite, UrlRewriteF}]} | Rewrite].

%% ---------------------------------------------------------------------------
%% Callback api
%% Reinventing the elli middelware "is export -> call"

-define(CALL(M, F, A),
        ?NRCLASS(apply(M, F, A),
                 list_to_binary([atom_to_list(M), ":",
                                 atom_to_list(F)]))).

preprocess(Req, Args) ->
    {Mod, CallbackArgs} = real_mod(Args),
    case erlang:function_exported(Mod, preprocess, 2) of
        true ->
            ?CALL(Mod, preprocess, [Req, CallbackArgs]);
        false ->
            Req
    end.

postprocess(Req, Res, Args) ->
    {Mod, CallbackArgs} = real_mod(Args),
    case erlang:function_exported(Mod, postprocess, 3) of
        true ->
            ?CALL(Mod, postprocess, [Req, Res, CallbackArgs]);
        false ->
            Res
    end.

handle_event(elli_startup = Event, EventArgs, Args) ->
    {Mod, _CallbackArgs} = real_mod(Args),
    code:ensure_loaded(Mod),
    do_handle_event(Event, EventArgs, Args);
handle_event(Event, EventArgs, Args) ->
    do_handle_event(Event, EventArgs, Args).

do_handle_event(Event, EventArgs, Args) ->
    {Mod, CallbackArgs} = real_mod(Args),
    case erlang:function_exported(Mod, handle_event, 3) of
        true ->
            ?CALL(Mod, handle_event, [Event, EventArgs, CallbackArgs]);
        false ->
            ok
    end.

handle(Req, Args) ->
    {Mod, CallbackArgs} = real_mod(Args),
    case erlang:function_exported(Mod, handle, 2) of
        true ->
            ?CALL(Mod, handle, [Req, CallbackArgs]);
        false ->
            ignore
    end.

real_mod(Args) ->
    {proplists:get_value(callback, Args),
     proplists:get_value(callback_args, Args)}.
