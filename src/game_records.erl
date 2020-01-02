-module(game_records).
-export([game_from_json/1]).
-include("game.hrl").

game_from_json(#{<<"title">> := Name,
                 <<"start">> := Start,
                 <<"moves">> := Moves,
                 <<"puzzles">> := Puzzles
                 } = _Json) ->
    #game{name = Name,
          start = start_from_list(Start),
          moves = tasks_from_map(Moves),
          puzzles = tasks_from_map(Puzzles)
         }.

start_from_list([<<"move">>, MoveName | _T]) ->
    {move, MoveName};
start_from_list([<<"puzzle">>, PuzzleName | _T]) ->
    {puzzle, PuzzleName}.
task_from_map(Name, #{<<"answer">> := Answer,
                  <<"assignment">> := Assignment,
                  <<"next_state">> := NextState}) ->
    #task{name = Name, assignment = messages_from_list(Assignment), answer = Answer, next_state = NextState}.

tasks_from_map(Puzzles) ->
    maps:map(fun task_from_map/2, Puzzles).

messages_from_list(List) ->
    lists:map(fun message_from_map/1, List).
message_from_map(#{<<"text">> := Body}) ->
    {text, Body};
message_from_map(#{<<"image">> := Path}) ->
    {image, Path}.
