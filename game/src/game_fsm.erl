%%%-------------------------------------------------------------------
%% @doc Main module for FSM like games. This module serves for handling 
%%      one instance of a game that user plays
%% @end
%%%-------------------------------------------------------------------

-module(game_fsm).
-behaviour(gen_statem).
-export([init/1,
         callback_mode/0,
         handle_event/4]).
-export([start_link/1,
         send_event/2]).

-include("game.hrl").

-record(state, {
          game :: #game{},
          start_state ,
          time0,
          game_log = [] :: [],
          points = 0 :: integer(),
          solved_puzzles_count = 0 :: integer()
         }).

%% @doc Number of points awarded for correctly solving a puzzle
-define(POINTS_PUZZLE_OK, 10).
%% @doc Numbber of points awarded for taking a hint. Usually negative number
-define(POINTS_HINT, -5).

%   {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka">>).
%   game_session:get_session(SessionHolder, mirek).


%   f(Pid), {ok, Pid, _} = game_fsm:start("../cibulka_game/definition.json").
%   game_fsm:send_event(Pid, <<"napoveda">>).

%% @doc start a game process, Argument is a path to file with a game definition
start_link(GameSpecPath) ->
    {ok, File} = file:read_file(GameSpecPath),
    Json = jiffy:decode(File, [return_maps]),
    GameSpec = game_records:game_from_json(Json),
    gen_statem:start_link(?MODULE, GameSpec, []).

%% @doc let the Pid process a message Msg encoded as a binary
send_event(Pid, Msg) ->
    case game_commands:command(Msg) of
        error -> [{text, <<"prikaz nerozpoznan, nevis-li co a jak napis '?'"/utf8>>}];
        Command -> gen_statem:call(Pid, Command)
    end.

%%%%%%%%%%%%%%
%% handlers %%
%%%%%%%%%%%%%%

%% @doc initialize a game process with #game{} record
init(#game{start = Start} = Game) ->
    io:format("Start: ~p~n", [Start]),
    {ok, Start, #state{game = Game, time0 = erlang:monotonic_time()}}.

%% @doc gen_statem function declaring inner working callback module
callback_mode() ->
    handle_event_function.

%% @doc gen_statem event handler of a form (EventOriginator, EventContent, StateOfFsm, ProcessState)
handle_event({call, From}, {yes}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    Hint = get_hint(StateToGo, Game),
    State2 = log(hint, StateToGo, State),
    {next_state, StateToGo, State2, [{reply, From, Hint}]};

handle_event({call, From}, {no}, {confirm_hint, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"tak se mi libis, ja verim, ze to das">>}]}]};

handle_event({call, From}, _, {confirm_hint, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"nerekl jsi, zda napovedu chces, nebo ne, takze jsem ti ji neposlal.
    pokud ji budes chtit, tak napis 'napoveda' a pak potvrd 'ano'">>}]}]};

handle_event({call, From}, {help}, _, #state{} = State) ->
    {keep_state, State, [{reply, From, [{text, game_commands:help()}]}]};

handle_event({call, From}, {score}, _, #state{time0 = StartTime, game_log = GameLog} = State) ->
    DurationSec = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, second),
    {Minutes, Seconds} = minutes_seconds(DurationSec),
    Points = score(State),
    {keep_state, State, [{reply, From, [
        {text, list_to_binary(io_lib:format("mas ~p bodu a hrajes ~p minut a ~p vterin", [Points, Minutes, Seconds]))}
                                       ]}]};

handle_event({call, From}, {assignment}, {move, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};
handle_event({call, From}, {assignment}, {puzzle, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};

handle_event({call, From}, {guess, Guess}, {move, _MoveName} = CurrentState, #state{game = Game} = State) ->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
            State2 = log(move_answer_ok, CurrentState, State),
            {next_state, NextState, State2, [{reply, From, [{text, <<"To bylo dobre!">>}] ++ NextAssignment}]};
        false ->
            State2 = log({move_answer_wrong, Guess}, CurrentState, State),
            {keep_state, State2, [{reply, From, [{text, <<"To nebylo dobre. Jsi na spravnem miste?">>}]}]}
    end;

handle_event({call, From}, {guess, Guess}, {puzzle, _PuzzleName} = CurrentState, #state{game = Game} = State) ->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
            State2 = log(puzzle_answer_ok, CurrentState, State),
            R = [{text, <<"To bylo dobre">>} | NextAssignment],
            {next_state, NextState, State2, [{reply, From, R}]};
        false ->
            State2 = log({puzzle_answer_wrong, Guess}, CurrentState, State),
            {keep_state, State2, [{reply, From, [{text, <<"To bylo spatne, promin">>}]}]}
    end;

handle_event({call, From}, {hint}, {puzzle, _PuzzleName} = CurrentState, #state{game = Game} = State) ->
    case hint_used(CurrentState, State) of
        true ->
            Hint = get_hint(CurrentState, Game),
            {keep_state, State, [{reply, From, [{text, <<"o napovedu uz sis zadal, bylo to:">>} | Hint]}]};
        false ->
            {next_state, {confirm_hint, CurrentState}, State, [{reply, From, [{question,<<"Opravdu chces vyuzit napovedu?">>}]}]}
    end;

handle_event({call, From}, {hint}, _CurrentState, State) ->
    {keep_state, State, [{reply, From, [{text,<<"K tomuhle napoveda neni">>}]}]};

handle_event({call, From}, {give_up}, {puzzle, _PuzzleName} = CurrentState, State) ->
    {next_state, {confirm_giveup, CurrentState}, State, [{reply, From, [{question,<<"Opravdu chces vzdat tento ukol a jit na dalsi?">>}]}]};

handle_event({call, From}, {give_up}, _CurrentState, State) ->
    {keep_state, State, [{reply, From, [{text,<<"tohle se vzdat neda....">>}]}]};

handle_event({call, From}, {yes}, {confirm_giveup, CurrentState}, #state{game = Game} = State) ->
    State2 = log(giveup, CurrentState, State),
    {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
    R = [{text, <<"Nevadi, jedeme dal...">>} | NextAssignment],
    {next_state, NextState, State2, [{reply, From, R}]};

handle_event({call, From}, {no}, {confirm_giveup, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"tak se mi libis, jen verim, ze to das">>}]}]};

handle_event({call, From}, _Event, finish, #state{} = State) ->
    State2 = log(finish, finish, State),
    {keep_state, State2, [{reply, From, [{text, <<"uz jsi v cili">>}]}]};
handle_event({call, From}, {hello}, CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, [{text, <<"vitej">>},{text, game_commands:help()} ] ++ Assignment }]};
% internal clause
handle_event(info, clear_state, _, _) ->
    {keep_state, #state{}, []};
handle_event({call, From}, Content, StateName, State) ->
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, [{reply, From, [{text, <<"tenhle prikaz ted nejde pouzit">>}]}]}.

%%%%%%%%%%%%%%%%%%%%%%%
%% private functions %%
%%%%%%%%%%%%%%%%%%%%%%%

%% @doc get the assignment for current task from game definition
get_assignment(finish, #game{}) ->
    [{text, <<"gratulujeme k absolvovani hry">>}];
get_assignment({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{assignment = Assignment}} = Moves,
    Assignment;
get_assignment({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{assignment = Assignment}} = Puzzles,
    Assignment.

%% @doc get correct answer for current task from game definition
get_answer({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{answer = Answer}} = Moves,
    Answer;
get_answer({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{answer = Answer}} = Puzzles,
    Answer.

%% @doc get next state of game by current state
get_next_state({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{next_state = NextState}} = Moves,
    NextState;
get_next_state({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{next_state = NextState}} = Puzzles,
    NextState.

%% @doc get hint for current puzzle
get_hint({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{hint = Hint}} = Puzzles,
    Hint.

%% @doc compound function called while correct answer was given. Return next state of the game and presents next assignment
get_next_assignment(CurrentState, Game) ->
    NextState = get_next_state(CurrentState, Game),
    Assignment = get_assignment(NextState, Game),
    {NextState, Assignment}.

%% @doc given number of seconds return a tuple with {Minutes, Seconds}
minutes_seconds(Seconds) ->
    {Seconds div 60, Seconds rem 60}.

%% score:

%% @doc write the event into game log attach a relative time
log(Event, StateName, #state{game_log = Log, time0 = Time0} = State) ->
    EventTime = erlang:monotonic_time() - Time0,
    State#state{game_log = [{EventTime, Event, StateName} | Log]}.

%% @doc tell whether hint was used in current state
hint_used(CurrentState, #state{game_log = GameLog}) ->
    length(lists:filter(fun({_, hint, CS}) when CS =:= CurrentState -> true;
                           (_) -> false end,
                        GameLog)) > 0.

%% @doc compute players score in the game
score(#state{game_log = GameLog}) ->
    PuzzleOKs = [PuzzleState || {_, puzzle_answer_ok, PuzzleState} <- GameLog],
    io:format("PuzzleOKs: ~p~n", [PuzzleOKs]),
    HintsUsed = [PuzzleState || {_, hint, PuzzleState} <- GameLog],
    io:format("HintsUsed: ~p~n", [HintsUsed]),
    HintsForOKPuzzle = [PuzzleState || PuzzleState <- HintsUsed, lists:member(PuzzleState, PuzzleOKs)],
    io:format("HintsForOKPuzzle: ~p~n", [HintsForOKPuzzle]),
    %TODO: rozepsat vysledky
    length(PuzzleOKs) * ?POINTS_PUZZLE_OK
    + length(HintsForOKPuzzle) * ?POINTS_HINT.
