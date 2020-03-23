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
                 recipient = _PageId, sender = Sender, content = Content,
                 type = _Type, timestamp = _Timestamp
                }) ->
    {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka">>),
    Session = game_session:get_session(SessionHolder, Sender),
    _Responses = game_fsm:send_event(Session, Content).
