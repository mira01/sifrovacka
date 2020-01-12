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
          start ,
          points = 0 :: integer(),
          solved_puzzles_count = 0 :: integer()
         }).


%   f(Pid), {ok, Pid} = game_fsm:start("../cibulka_game/definition.json").
%   game_fsm:send_event(Pid, <<"napoveda">>).

start(GameSpecPath) ->
    {ok, File} = file:read_file(GameSpecPath),
    Json = jiffy:decode(File, [return_maps]),
    GameSpec = game_records:game_from_json(Json),
    Response = gen_statem:start(?MODULE, GameSpec, []),
    case Response of
        {ok, Pid} ->
            {ok, Pid, send_event(Pid, <<"ahoj">>)};
        _ -> Response
    end.

send_event(Pid, Msg) ->
    case game_commands:command(Msg) of
        error -> {text, <<"prikaz nerozpoznan, nevis-li co a jak napis '?'"/utf8>>};
        Command -> gen_statem:call(Pid, Command)
    end.

% handlers
init(#game{start = Start} = Game) ->
    io:format("Start: ~p~n", [Start]),
    {ok, Start, #state{game = Game, start = erlang:monotonic_time()}}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, {yes}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    Hint = get_hint(StateToGo, Game),
    {next_state, StateToGo, State, [{reply, From, Hint}]};
    
handle_event({call, From}, {no}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"tak se mi libis, jen verim, ze to das">>}]};

handle_event({call, From}, _, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"nerekl jsi, zda napovedu chces, nebo ne, takze jsem ti ji neposlal.
    pokud ji budes chtit, tak napis 'napoveda' a pak potvrd">>}]};

handle_event({call, From}, {help}, _, #state{} = State) ->
    {keep_state, State, [{reply, From, game_commands:help()}]};
handle_event({call, From}, {score}, _, #state{points = Points, start = StartTime} = State) ->
    DurationSec = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, second),
    {Minutes, Seconds} = minutes_seconds(DurationSec),
    {keep_state, State, [{reply, From, {text, io_lib:format("mas ~p bodu a hrajes ~p minut a ~p vterin", [Points, Minutes, Seconds])}}]};


handle_event({call, From}, {assignment}, {move, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};
handle_event({call, From}, {assignment}, {puzzle, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};

handle_event({call, From}, {guess, Guess}, {move, MoveName} = CurrentState, #state{game = Game} = State) ->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
            {next_state, NextState, State, [{reply, From, [{text, <<"move ok: to bylo dobre">>}] ++ NextAssignment}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;

handle_event({call, From}, {guess, Guess}, {puzzle, PuzzleName} = CurrentState, #state{game = Game} = State) ->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
            {next_state, NextState, State, [{reply, From, [<<"puzzle ok: to bylo dobre">>] ++ NextAssignment}]};
        false ->
            {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]}
    end;

handle_event({call, From}, {hint}, {puzzle, PuzzleName} = CurrentState, #state{game = Game} = State) ->
    {next_state, {confirm_hint, CurrentState}, State, [{reply, From, {question,<<"Opravdu chces vyuzit napovedu?">>}}]};

handle_event({call, From}, {hint}, CurrentState, #state{game = Game} = State) ->
    {keep_state, State, [{reply, From, {text,<<"Napovedy mame jen pro sifry">>}}]};

handle_event({call, From}, {give_up}, {puzzle, PuzzleName} = CurrentState, #state{game = Game} = State) ->
    {next_state, {confirm_giveup, CurrentState}, State, [{reply, From, {question,<<"Opravdu chces vzdat tento ukol a jit na dalsi?">>}}]};

handle_event({call, From}, {giveup}, CurrentState, #state{game = Game} = State) ->
    {keep_state, State, [{reply, From, {text,<<"tohle se vzdat neda....">>}}]};

handle_event({call, From}, {yes}, {confirm_giveup, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"skoda...">>}]};
    
handle_event({call, From}, {no}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    {next_state, StateToGo, State, [{reply, From, <<"tak se mi libis, jen verim, ze to das">>}]};

handle_event({call, From}, Event, finish, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"uz jsi v cili">>}]};
handle_event({call, From}, {hello}, CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, [{text, <<"vitej">>},{text, game_commands:help()} ] ++ Assignment }]};
handle_event(info, clear_state, _, _) ->
    {keep_state, #state{}, []};
handle_event({call, From}, Content, StateName, State) ->
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, [{reply, From, [{text, <<"tenhle prikaz ted nejde pouzit">>}]}]}.

%% private functions

get_assignment(finish, #game{}) ->
    [{text, <<"gratulujeme k absolvovani hry">>}];
get_assignment({move, MoveName}, #game{moves = Moves} = Game) ->
    #{MoveName := #task{assignment = Assignment}} = Moves,
    Assignment;
get_assignment({puzzle, PuzzleName}, #game{puzzles = Puzzles} = Game) ->
    #{PuzzleName := #task{assignment = Assignment}} = Puzzles,
    Assignment.

get_answer({move, MoveName}, #game{moves = Moves} = Game) ->
    #{MoveName := #task{answer = Answer}} = Moves,
    Answer;
get_answer({puzzle, PuzzleName}, #game{puzzles = Puzzles} = Game) ->
    #{PuzzleName := #task{answer = Answer}} = Puzzles,
    Answer.

get_next_state({move, MoveName}, #game{moves = Moves} = Game) ->
    #{MoveName := #task{next_state = NextState}} = Moves,
    NextState;
get_next_state({puzzle, PuzzleName}, #game{puzzles = Puzzles} = Game) ->
    #{PuzzleName := #task{next_state = NextState}} = Puzzles,
    NextState.

get_hint({puzzle, PuzzleName}, #game{puzzles = Puzzles} = Game) ->
    #{PuzzleName := #task{hint = Hint}} = Puzzles,
    Hint.

get_next_assignment(CurrentState, Game) ->
    NextState = get_next_state(CurrentState, Game),
    Assignment = get_assignment(NextState, Game),
    {NextState, Assignment}.

minutes_seconds(Seconds) ->
    {Seconds div 60, Seconds rem 60}.
