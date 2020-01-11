-module(game_fsm).
-behaviour(gen_statem).
-export([init/1,
         callback_mode/0,
         handle_event/4]).
-export([start/1,
         send_event/2]).

-include("game.hrl").

-record(state, {
          game :: #game{},
            points = 0 :: integer()
         }).

%   f(Pid), {ok, Pid} = game_fsm:start("../cibulka_game/definition.json").
%   game_fsm:send_event(Pid, <<"napoveda">>).

start(GameSpecPath) ->
    {ok, File} = file:read_file(GameSpecPath),
    Json = jiffy:decode(File, [return_maps]),
    GameSpec = game_records:game_from_json(Json),
    gen_statem:start(?MODULE, GameSpec, []).

send_event(Pid, Msg) ->
    case game_commands:command(Msg) of
        error -> {text, <<"prikaz nerozpoznan, nevis-li co a jak napis '?'"/utf8>>};
        Command -> gen_statem:call(Pid, Command)
    end.

% handlers
init(#game{start = Start} = Game) ->
    io:format("Start: ~p~n", [Start]),
    {ok, Start, #state{game = Game}}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, {help}, _, #state{} = State) ->
    {keep_state, State, [{reply, From, game_commands:help()}]};

handle_event({call, From}, {assignment}, {move, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};
handle_event({call, From}, {assignment}, {puzzle, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};

handle_event({call, From}, {guess, Guess}, {move, MoveName}, #state{game = Game} = State) ->
    io:format("Guess: ~p~n", [Guess]),
    Answer = get_answer(get_task_definition(MoveName, get_moves(State))),
    io:format("Answer: ~p~n", [Answer]),
    case Guess =:= Answer of
        true ->
            NextState = get_next_state(get_task_definition(MoveName, get_moves(State))),
            Assignment = get_assignment(NextState, Game),
            {next_state, NextState, State, [{reply, From, [{text, <<"move ok: to bylo dobre">>}] ++ Assignment}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;

handle_event({call, From}, {guess, Guess}, {puzzle, PuzzleName}, #state{game = Game} = State) ->
    io:format("Guess: ~p~n", [Guess]),
    Answer = get_answer(get_task_definition(PuzzleName, get_puzzles(State))),
    io:format("Answer: ~p~n", [Answer]),
    case Guess =:= Answer of
        true ->
            NextState = get_next_state(get_task_definition(PuzzleName, get_puzzles(State))),
            Assignment = get_assignment(NextState, Game),
            {next_state, NextState, State, [{reply, From, [<<"puzzle ok: to bylo dobre">>] ++ Assignment}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;

handle_event({call, From}, {hint}, {puzzle, PuzzleName} = CurrentState, #state{game = Game} = State) ->
    {next_state, {confirm_hint, CurrentState}, State, [{reply, From, {question,<<"Opravdu chces vyuzit napovedu?">>}}]};

handle_event({call, From}, {hint}, CurrentState, #state{game = Game} = State) ->
    {keep_state, State, [{reply, From, {text,<<"Napovedy mame jen pro sifry">>}}]};

handle_event({call, From}, {yes}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"bude to neco jako osm">>}]};
    
handle_event({call, From}, {no}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"tak se mi libis, jen verim, ze to das">>}]};

handle_event({call, From}, Event, finish, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"uz jsi v cili">>}]};
handle_event({call, From}, Content, StateName, State) ->
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, [{reply, From, <<"tenhle prikaz ted nejde pouzit">>}]}.

get_assignment(finish, #game{}) ->
    [{text, <<"gratulujeme k absolvovani hry">>}];
get_assignment({move, MoveName}, #game{moves = Moves} = Game) ->
    #{MoveName := #task{assignment = Assignment}} = Moves,
    Assignment;
get_assignment({puzzle, PuzzleName}, #game{puzzles = Puzzles} = Game) ->
    #{PuzzleName := #task{assignment = Assignment}} = Puzzles,
    Assignment.

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
    
    

