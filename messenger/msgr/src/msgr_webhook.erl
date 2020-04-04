-module(msgr_webhook).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("messages.hrl").

out(#arg{req = #http_request{method='GET'}, querydata = undefined}) ->
    [{status, 400},
     {content, "text/plain", "credentials expected"}];
    
out(#arg{req = #http_request{method='GET'}, querydata = Q, opaque = Opaque}) ->
    {verify_token, RequiredToken} = proplists:lookup(verify_token, Opaque),
    io:format("Q: ~p~n", [Q]),

    Params = uri_string:dissect_query(Q),
    io:format("Params: ~p~n", [Params]),
    Mode = proplists:get_value("hub.mode", Params),
    VerifyToken = proplists:get_value("hub.verify_token", Params),
    Challenge = proplists:get_value("hub.challenge", Params),

    if (Mode =:= "subscribe"
        andalso RequiredToken =:= VerifyToken
        andalso Challenge =/= undefined
       ) ->
           [{status, 200},
            {content, "text/plain", Challenge}];
       true ->
           [{status, 400},
            {content, "text/plain", "no valid credentials provided"}]
    end;


out(#arg{req = _Req, clidata = Clidata, querydata = _Q, opaque = Opaque}) ->
    io:format("Req data: ~p~n", [Clidata]),
    %{abs_path, AbsPath} = (Arg#arg.req)#http_request.path,
    Events = msgr_event_parser:parse_event(Clidata),
    Callback = proplists:get_value(callback, Opaque),
    Resp = lists:foldl(fun(E, Resps) ->
                               R = process_event(E, Callback),
                               Resps ++ R
                       end, [], Events),
    io:format("Resp: ~p~n", [Resp]),
    [{status, 200},
     {content, "text/plain", io_lib:format("~p~n", [Resp])}].

process_event(#event{
                 recipient = PageId, sender = Sender, content = Content,
                 type = unknown_message, timestamp = _Timestamp
                }, _Callback) ->
    {ok, SenderPid} = msgr_sender_sup:sender(PageId),
    msgr:send(SenderPid, Sender, {text, <<"tenhle typ zpravy neumime">>});

process_event(#event{
                 recipient = PageId, sender = Sender, content = Content,
                 type = _Type, timestamp = _Timestamp
                } = Event, Callback) when is_function(Callback) ->
    {ok, MessageSenderPid} = msgr_sender_sup:sender(PageId),
    Responses = Callback(Event),
    lists:foreach(fun(R) -> msgr:send(MessageSenderPid, Sender, R) end, Responses).
