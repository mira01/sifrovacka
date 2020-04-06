-module(starter).

-export([start_link/0]).
-include_lib("msgr/include/messages.hrl").

start_link() ->
    % Sending messages:
    {ok, AppId} = application:get_env(app_id),
    {ok, AccessToken} = application:get_env(access_token),
    {ok, Endpoint} = application:get_env(endpoint),

    {ok, Sender} = msgr_sender_sup:start_sender(AppId, AccessToken, Endpoint),

    Callback = fun(#event{
                      recipient = PageId, sender = Sender, content = Content,
                      type = _Type, timestamp = _Timestamp
                     } = Event) ->
                       {ok, SessionHolder} = game_session_sup:sessions(page_id2game(PageId)),
                       {ok, SenderPid} = msgr_sender_sup:sender(PageId),
                       Session = game_session:get_session(SessionHolder, Sender),
                       game_fsm:send_event(Session, Content)
               end,
    
    {ok, VerifyToken} = application:get_env(verify_token),
    {ok, YawsGconfList} = application:get_env(yaws_gconf_list),
    {ok, YawsSconfList} = application:get_env(yaws_sconf_list),

    Opaque = proplists:get_value(opaque, YawsSconfList, []),
    OpaqueFilled = Opaque ++ [{callback, Callback}, {verify_token, VerifyToken}],
    YawsSconfListFilled = [{opaque, OpaqueFilled} | YawsSconfList],
    io:format("YawsSconfListFilled: ~p~n", [YawsSconfListFilled]),

    {docroot, DocRoot} = proplists:lookup(docroot, YawsSconfListFilled),
    {id, Id} = proplists:lookup(id, YawsGconfList),

    Webhook = msgr_sup:start_yaws(DocRoot, YawsSconfListFilled, YawsGconfList, Id),
    {ok, self()}.
    

page_id2game(PageId) ->
    %TODO
    <<"cibulka">>.
