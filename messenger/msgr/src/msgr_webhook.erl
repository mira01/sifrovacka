-module(msgr_webhook).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("messages.hrl").

out(#arg{req = #http_request{method='GET'}, querydata = Q}) ->
    Params = uri_string:dissect_query(Q),
    "subscribe" = proplists:get_value("hub.mode", Params),
    "lhota_trophy" = proplists:get_value("hub.verify_token", Params),
    Challenge = proplists:get_value("hub.challenge", Params),
    [{status, 200},
     {content, "text/plain", Challenge}];

out(#arg{req = _Req, clidata = Clidata, querydata = _Q}) ->
    io:format("Req data: ~p~n", [Clidata]),
    %{abs_path, AbsPath} = (Arg#arg.req)#http_request.path,
    Events = msgr_event_parser:parse_event(Clidata),
    Resp = lists:foldl(fun(E, Resps) ->
                               R = process_event(E),
                               Resps ++ R
                       end, [], Events),
    io:format("Resp: ~p~n", [Resp]),
    [{status, 200},
     {content, "text/plain", io_lib:format("~p~n", [Resp])}].

process_event(#event{
                 recipient = PageId, sender = Sender, content = Content,
                 type = unknown_message, timestamp = _Timestamp
                }) ->
    {ok, SenderPid} = msgr_sender_sup:sender(PageId),
    msgr:send(SenderPid, Sender, {text, <<"tenhle typ zpravy neumime">>});

process_event(#event{
                 recipient = PageId, sender = Sender, content = Content,
                 type = _Type, timestamp = _Timestamp
                }) ->
    {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka">>),
    {ok, SenderPid} = msgr_sender_sup:sender(PageId),
    Session = game_session:get_session(SessionHolder, Sender),
    Responses = game_fsm:send_event(Session, Content),
    lists:foreach(fun(R) -> msgr:send(SenderPid, Sender, R) end, Responses).
