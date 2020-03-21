-module(game_event_parser).

-export([parse_event/1]).
-include("game.hrl").

parse_event(Payload) ->
    Json = jiffy:decode(Payload, [return_maps]),
    #{<<"entry">> := [Entry]} = Json,
    parse_entry(Entry).

parse_entry(#{<<"messaging">> := Messages}) ->
    lists:map(fun(M) -> parse_message(M) end , Messages).

parse_message(#{
                <<"sender">> := #{<<"id">> := Sender},
                <<"recipient">> := #{<<"id">> := PageId},
                <<"timestamp">> := Timestamp,
                <<"message">> := #{<<"text">> := Text}
                }) ->
    #event{type = text_message,
           timestamp = Timestamp,
           sender = Sender,
           recipient = PageId,
           content = Text}.

