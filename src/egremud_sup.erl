%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{conn_sup,
              {egremud_conn_sup, start_link, []},
              permanent,
              brutal_kill,
              supervisor,
              [egremud_conn_sup]},
             {egremud_event_log,
              {egremud_event_log, start_link, []},
              permanent,
              brutal_kill,
              worker,
              [egremud_event_log]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
