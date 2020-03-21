-module(game_webhook).
-export([out/1]).

-compile([export_all]).

-include_lib("yaws/include/yaws_api.hrl").
-include("game.hrl").

out(#arg{req = #http_request{method='GET'} = Req, querydata = Q} = Arg) ->
    Params = uri_string:dissect_query(Q),
    "subscribe" = proplists:get_value("hub.mode", Params),
    "lhota_trophy" = proplists:get_value("hub.verify_token", Params),
    Challenge = proplists:get_value("hub.challenge", Params),
    [{status, 200},
     {content, "text/plain", Challenge}];

out(#arg{req = Req, clidata = Clidata, querydata = Q} = Arg) ->
    io:format("Req data: ~p~n", [Clidata]),
    %{abs_path, AbsPath} = (Arg#arg.req)#http_request.path,
    Events = game_event_parser:parse_event(Clidata),
    Resp = lists:foldl(fun(E, Resps) ->
                               R = process_event(E),
                               Resps ++ R
                       end, [], Events),
    io:format("Resp: ~p~n", [Resp]),
    [{status, 200},
     {content, "text/plain", io_lib:format("~p~n", [Resp])}].

process_event(#event{
                 recipient = PageId, sender = Sender, content = Content,
                 type = Type, timestamp = Timestamp
                }) ->
    {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka">>),
    Session = game_session:get_session(SessionHolder, Sender),
    Responses = game_fsm:send_event(Session, Content).
