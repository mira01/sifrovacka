-module(msgr_yaws_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    YBed = {msgr_yaws, {msgr_yaws,start,[]},
            temporary,2000,worker,[msgr_yaws]},
    {ok,{{one_for_all,0,1}, [YBed]}}.
