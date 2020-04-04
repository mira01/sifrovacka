%%%-------------------------------------------------------------------
%% @doc msgr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(msgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_yaws/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_yaws(DocRoot, SconfList, GconfList, Id) ->
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(DocRoot, SconfList, GconfList, Id),
    [supervisor:start_child(msgr_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 1, 1}, [
                                #{id => msgr_yaws_sup
                                 ,start => {msgr_yaws_sup, start_link, []}
                                 }
                                ,
                                #{id => msgr_sender_sup
                                 ,start => {msgr_sender_sup, start_link, []}
                                 }
                               ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
