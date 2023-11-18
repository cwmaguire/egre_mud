%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{object_sup,
              {gerlshmud_object_sup, start_link, []},
              permanent,
              brutal_kill,
              supervisor,
              [gerlshmud_object_sup]},
             {conn_sup,
              {gerlshmud_conn_sup, start_link, []},
              permanent,
              brutal_kill,
              supervisor,
              [gerlshmud_conn_sup]},
             {egremud_event_log,
              {egremud_event_log, start_link, []},
              permanent,
              brutal_kill,
              worker,
              [gerlshmud_event_log]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
