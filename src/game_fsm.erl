-module(game_fsm).
-behaviour(gen_statem).
-export([init/1,
         callback_mode/0,
         handle_event/4]).
-export([start/1,
         send_event/2]).

-include("game.hrl").


-define(GUESS, <<"heslo je ">>).

-record(state, {
          game :: #game{},
            points = 0 :: integer()
         }).

start(GameSpecPath) ->
    {ok, File} = file:read_file(GameSpecPath),
    Json = jiffy:decode(File, [return_maps]),
    GameSpec = game_records:game_from_json(Json),
    gen_statem:start(?MODULE, GameSpec, []).

send_event(Pid, Msg) ->
    gen_statem:call(Pid, Msg).

% handlers
init(#game{start = Start} = Game) ->
    io:format("Start: ~p~n", [Start]),
    {ok, Start, #state{game = Game}}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, <<"reseni ", Guess/binary>>, {move, MoveName}, State) ->
    io:format("Guess: ~p~n", [Guess]),
    Answer = get_answer(get_task_definition(MoveName, get_moves(State))),
    io:format("Answer: ~p~n", [Answer]),
    case Guess =:= Answer of
        true ->
            NextState = get_next_state(get_task_definition(MoveName, get_moves(State))),
            io:format("NextState: ~p~n", [NextState]),
            {next_state, NextState, State, [{reply, From, <<"move ok: to bylo dobre">>}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;

handle_event({call, From}, <<"reseni ", Guess/binary>>, {puzzle, PuzzleName}, State) ->
    io:format("Guess: ~p~n", [Guess]),
    Answer = get_answer(get_task_definition(PuzzleName, get_puzzles(State))),
    io:format("Answer: ~p~n", [Answer]),
    case Guess =:= Answer of
        true ->
            NextState = get_next_state(get_task_definition(PuzzleName, get_puzzles(State))),
            io:format("NextState: ~p~n", [NextState]),
            {next_state, NextState, State, [{reply, From, <<"puzzle ok: to bylo dobre">>}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;
handle_event({call, From}, Event, {puzzle, MoveName}, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"to neni spravny kod">>}]};
handle_event({call, From}, Event, finish, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"uz jsi v cili">>}]};
handle_event(Type, Content, StateName, State) ->
    io:format("Type: ~p~n", [Type]),
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, []}.

get_moves(#state{game = #game{moves = Moves}}) ->
    Moves.
get_puzzles(#state{game = #game{puzzles = Puzzles}}) ->
    Puzzles.
get_task_definition(Key, Map) ->
    #{Key := Definition} = Map,
    Definition.
get_answer(#task{answer = Answer}) ->
    Answer.
get_next_state(#task{next_state = NextState}) ->
    NextState.
    
    

