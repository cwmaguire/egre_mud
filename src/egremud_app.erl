%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_app).
-behaviour(application).

-include("egremud.hrl").

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_Type, _Args) ->
    Port = case application:get_env(gerlshmud, port) of
               {ok, EnvPort} ->
                   EnvPort;
               _ ->
                   8080
           end,

    Paths = [{"/", gerlshmud_websocket, ?NO_OPTIONS},
             {"/log", gerlshmud_websocket_log, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, gerlshmud, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_clear(gerlshmud_http_listener,
                           [{port, Port}],
                           #{env => #{dispatch => Dispatch}}),
    egremud_sup:start_link().

stop(_State) ->
	ok.
