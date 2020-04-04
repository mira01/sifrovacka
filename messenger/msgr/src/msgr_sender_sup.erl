%%%-------------------------------------------------------------------
%% @doc supervisor for senders (typicaly one sender per facebook page)
%% @end
%%%-------------------------------------------------------------------

-module(msgr_sender_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, sender/1, start_sender/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_sender(AppId, AccessToken, Endpoint) ->
    Spec = #{id => AppId
      ,start => {msgr, start_link, [AccessToken, Endpoint]}
      ,restart => permanent
      ,type => worker
    },
    supervisor:start_child(?MODULE, Spec).

sender(Name) ->
    Children = supervisor:which_children(?SERVER),
    sender(Name, Children).
sender(_Name, []) ->
    undefined;
sender(Name, [{Name, Child, _Type, _Modules} | _Tail]) ->
    {ok, Child};
sender(Name, [_|Tail]) ->
    sender(Name, Tail).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 1, 1}, [] }}.

%%====================================================================
%% Internal functions
%%====================================================================
