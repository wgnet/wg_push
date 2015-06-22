-module(wg_push_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


-spec(start_link() -> {ok, pid()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

    Sender = {wg_push_sender,
              {wg_push_sender, start_link, []},
              Restart, Shutdown, worker,
              [wg_push_sender]},

    {ok, {SupFlags, [Sender]}}.
