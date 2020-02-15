%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_sup).

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
    {ok, {{one_for_all, 1, 1},[
                               #{id => game_yaws_sup
                                ,start => {game_yaws_sup, start_link, []}
                                }
                              ,#{id => game_session_sup
                                ,start => {game_session_sup, start_link, [
                                      {<<"cibulka">>, <<"../cibulka_game/definition.json">>}
                                    ]}
                               }
                              ]}}.
