%%%-------------------------------------------------------------------
%% @doc supervisor for senders (typicaly one sender per facebook page)
%% @end
%%%-------------------------------------------------------------------

-module(msgr_sender_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, sender/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    io:format("start_link: ~p~n", [start_link]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

sender(Name) ->
    io:format("Name: ~p~n", [Name]),
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
    io:format("init: ~p~n", [init]),
    AccessToken = "EAAGp7kKPZBm0BADujuqeVXzjndeQyNSrXrytOk0qxL4PcUAv704zQkOhKAu5VdbNdqAYjZC77iNnMbcGrfMDGQZCDwJnKo8Hbw7do2F7yDEdLMEJHLko604sgpEcaxENJT0zsaS0KLSiVNIY9gucw9pGUhKOQ2iRggN2ZCYPkoJVajcnlAva",
    Endpoint = "https://graph.facebook.com/v2.6/me/messages",

    {ok, {{one_for_one, 1, 1}, [
                                #{id => <<"109821290605511">>
                                 ,start => {msgr, start_link, [AccessToken, Endpoint]}
                                 ,restart => permanent
                                 ,type => worker
                                 }
                               ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

