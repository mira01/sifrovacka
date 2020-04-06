%%%-------------------------------------------------------------------
%% @doc sifrovacka_release top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sifrovacka_release_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, [#{id => starter,
                                  start => {starter, start_link, []},
                                  restart => temporary
                                  }
                               ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
