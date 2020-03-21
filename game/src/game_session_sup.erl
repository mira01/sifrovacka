%%%-------------------------------------------------------------------
%% @doc supervisor of session storages
%% @end
%%%-------------------------------------------------------------------

-module(game_session_sup).

-behaviour(supervisor).

-compile([axport_all]).
%% API
-export([start_link/1, sessions/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Games) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Games]).

sessions(Name) ->
    Children = supervisor:which_children(?SERVER),
    sessions(Name, Children).
sessions(_Name, []) ->
    undefined;
sessions(Name, [{Name, Child, _Type, _Modules} | _Tail]) ->
    {ok, Child};
sessions(Name, [_|Tail]) ->
    sessions(Name, Tail).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Games) ->
    {ok, {{one_for_one, 1, 1},[
                               #{id => Name 
                                ,start => {game_session, start_link, [Name, Path]}
                                }
                              || {Name, Path} <- Games]
         }}.
