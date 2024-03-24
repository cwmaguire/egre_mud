%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_Type, _Args) ->
    Port = case application:get_env(egremud, port) of
               {ok, EnvPort} ->
                   EnvPort;
               _ ->
                   8080
           end,

    Paths = [{"/", egremud_websocket, ?NO_OPTIONS},
             {"/log", egremud_websocket_log, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, egremud, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_clear(egremud_http_listener,
                           [{port, Port}],
                           #{env => #{dispatch => Dispatch}}),
    egremud_sup:start_link().

stop(_State) ->
	ok.
