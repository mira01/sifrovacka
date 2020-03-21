-module(game_records).
-export([game_from_json/1]).
-include("game.hrl").

game_from_json(#{<<"title">> := Name,
                 <<"start">> := Start,
                 <<"moves">> := Moves,
                 <<"puzzles">> := Puzzles
                 } = _Json) ->
    Game = #game{name = Name,
          start = start_from_list(Start),
          moves = moves_from_map(Moves),
          puzzles = puzzles_from_map(Puzzles)
         },
    [] = validate(Game),
    Game.

start_from_list([<<"move">>, MoveName | _T]) ->
    {move, MoveName};
start_from_list([<<"puzzle">>, PuzzleName | _T]) ->
    {puzzle, PuzzleName}.
move_from_map(Name, #{<<"answer">> := Answer
                     ,<<"assignment">> := Assignment
                     ,<<"next_state">> := NextState}
                    ) ->
    #task{name = Name,
          assignment = messages_from_list(Assignment),
          answer = string:casefold(Answer),
          next_state = next_state_from_json(NextState)}.

moves_from_map(Puzzles) ->
    maps:map(fun move_from_map/2, Puzzles).

puzzle_from_map(Name, #{<<"answer">> := Answer
                     ,<<"assignment">> := Assignment
                     ,<<"next_state">> := NextState
                     ,<<"hint">> := Hint}
                    ) ->
    #task{name = Name
         ,assignment = messages_from_list(Assignment)
         ,answer = string:casefold(Answer)
         ,next_state = next_state_from_json(NextState)
         ,hint = messages_from_list(Hint)
         }.

puzzles_from_map(Puzzles) ->
    maps:map(fun puzzle_from_map/2, Puzzles).

messages_from_list(List) ->
    lists:map(fun message_from_map/1, List).
message_from_map(#{<<"text">> := Body}) ->
    {text, Body};
message_from_map(#{<<"image">> := Path}) ->
    {image, Path}.

next_state_from_json(#{<<"move">> := Move}) ->
    {move, Move};
next_state_from_json(#{<<"puzzle">> := Puzzle}) ->
    {puzzle, Puzzle};
next_state_from_json(<<"finish">>) ->
    finish.

% validations

rule_start_is_valid(#game{start = {move, StartMove}, moves = Moves}) ->
    {maps:is_key(StartMove, Moves), <<"start does not point to valid move">>};
rule_start_is_valid(#game{start = {puzzle, StartPuzzle}, puzzles = Puzzles}) ->
    {maps:is_key(StartPuzzle, Puzzles), <<"start does not point to valid puzzle">>}.
rule_valid_next_states(#game{moves = Moves, puzzles = Puzzles} = Game) ->
    tasks_has_valid_next_states(Game, "moves", Moves)
    ++ tasks_has_valid_next_states(Game, "puzzle", Puzzles).

tasks_has_valid_next_states(#game{moves = Moves, puzzles = Puzzles}, TaskKind, Tasks) ->
    maps:fold(fun(K, #task{next_state = NextState}, Acc) ->
                      R = case next_state_is_valid(NextState, Moves, Puzzles) of
                              true -> {true, <<"">>};
                              _ -> {false, iolist_to_binary(io_lib:format("~p ~p points to invalid next state ~p", [TaskKind, K, NextState]))}
                          end,
                      [R|Acc]
              end,
              [], Tasks).

next_state_is_valid({move, Move}, AllMoves, _) ->
    maps:is_key(Move, AllMoves);
next_state_is_valid({puzzle, Puzzle}, _, AllPuzzles) ->
    maps:is_key(Puzzle, AllPuzzles);
next_state_is_valid(finish, _, _) ->
    true.

all_rules(Game) ->
    lists:flatten([
     rule_start_is_valid(Game)
     ,rule_valid_next_states(Game)
     % TODO: finish reachable
     % TODO: images exists
    ]).

validate(#game{} = Game) ->
    [ MaybeError || {false, MaybeError} <- all_rules(Game) ].
