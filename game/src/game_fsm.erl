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

-define(POINTS_PUZZLE_OK, 10).
-define(POINTS_HINT, -5).

%   {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka"/utf8>>).
%   game_session:get_session(SessionHolder, mirek).


%   f(Pid), {ok, Pid, _} = game_fsm:start("../cibulka_game/definition.json").
%   game_fsm:send_event(Pid, <<"napoveda"/utf8>>).

start_link(GameSpecPath) ->
    {ok, File} = file:read_file(GameSpecPath),
    Json = jiffy:decode(File, [return_maps]),
    GameSpec = game_records:game_from_json(Json),
    gen_statem:start_link(?MODULE, GameSpec, []).

send_event(Pid, Msg) ->
    case game_commands:command(Msg) of
        error -> [{text, <<"Příkaz nerozpoznán, nevíš-li co a jak, napiš '?'"/utf8>>}];
        Command -> gen_statem:call(Pid, Command)
    end.

% handlers
init(#game{start = Start} = Game) ->
    io:format("Start: ~p~n", [Start]),
    {ok, Start, #state{game = Game, time0 = erlang:monotonic_time()}}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, {yes}, {confirm_hint, StateToGo}, #state{game = Game} = State) ->
    Hint = get_hint(StateToGo, Game),
    State2 = log(hint, StateToGo, State),
    {next_state, StateToGo, State2, [{reply, From, Hint}]};

handle_event({call, From}, {no}, {confirm_hint, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"Tak se mi líbíš, já věřím, že to dáš"/utf8>>}]}]};

handle_event({call, From}, _, {confirm_hint, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"Neřekla jsi, jestli nápovědu chceš nebo ne, tak jsem ti ji neposlal.Pokud ji budeš chtít, napiš 'nápověda' a pak 'ano'"/utf8>>}]}]};

handle_event({call, From}, {help}, _, #state{} = State) ->
    {keep_state, State, [{reply, From, [{text, game_commands:help()}]}]};
handle_event({call, From}, {score}, _, #state{time0 = StartTime, game_log = GameLog} = State) ->
    DurationSec = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, second),
    {Minutes, Seconds} = minutes_seconds(DurationSec),
    Points = score(State),
    {keep_state, State, [{reply, From, [
        {text, list_to_binary(io_lib:format("Máš ~p bodů a hraješ ~p minut a ~p vteřin", [Points, Minutes, Seconds]))}
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
            {next_state, NextState, State2, [{reply, From, [{text, <<"Skvěle!"/utf8>>}] ++ NextAssignment}]};
        false ->
            State2 = log({move_answer_wrong, Guess}, CurrentState, State),
            {keep_state, State2, [{reply, From, [{text, <<"To nebylo správně"/utf8>>}]}]}
    end;

handle_event({call, From}, {guess, Guess}, {puzzle, _PuzzleName} = CurrentState, #state{game = Game} = State) ->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
            State2 = log(puzzle_answer_ok, CurrentState, State),
            R = [{text, <<"Výborně! Dobrá práce"/utf8>>} | NextAssignment],
            {next_state, NextState, State2, [{reply, From, R}]};
        false ->
            State2 = log({puzzle_answer_wrong, Guess}, CurrentState, State),
            {keep_state, State2, [{reply, From, [{text, <<"Bohužel toto není správně"/utf8>>}]}]}
    end;

handle_event({call, From}, {hint}, {puzzle, _PuzzleName} = CurrentState, #state{game = Game} = State) ->
    case hint_used(CurrentState, State) of
        true ->
            Hint = get_hint(CurrentState, Game),
            {keep_state, State, [{reply, From, [{text, <<"O nápovědu sis už žádala, bylo to: "/utf8>>} | Hint]}]};
        false ->
            {next_state, {confirm_hint, CurrentState}, State, [{reply, From, [{question,<<"Opravdu chceš využít nápovědu?"/utf8>>}]}]}
    end;

handle_event({call, From}, {hint}, _CurrentState, State) ->
    {keep_state, State, [{reply, From, [{text,<<"Pro tohle nemáme nápovědu"/utf8>>}]}]};

handle_event({call, From}, {give_up}, {puzzle, _PuzzleName} = CurrentState, State) ->
    {next_state, {confirm_giveup, CurrentState}, State, [{reply, From, [{question,<<"Opravdu chceš vzdát tento úkol a jít na další?"/utf8>>}]}]};

handle_event({call, From}, {give_up}, _CurrentState, State) ->
    {keep_state, State, [{reply, From, [{text,<<"Tohle se přeskočit nedá...."/utf8>>}]}]};

handle_event({call, From}, {yes}, {confirm_giveup, CurrentState}, #state{game = Game} = State) ->
    State2 = log(giveup, CurrentState, State),
    {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
    R = [{text, <<"Nevadi, jedeme dal..."/utf8>>} | NextAssignment],
    {next_state, NextState, State2, [{reply, From, R}]};

handle_event({call, From}, {no}, {confirm_giveup, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"No už jsem se lekl. Pojď, to dáš."/utf8>>}]}]};

handle_event({call, From}, _Event, finish, #state{} = State) ->
    State2 = log(finish, finish, State),
    {keep_state, State2, [{reply, From, [{text, <<"Už jsi v cíli. Díky za hru."/utf8>>}]}]};
handle_event({call, From}, {hello}, CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, [{text, <<"Vítej"/utf8>>},{text, game_commands:help()} ] ++ Assignment }]};
handle_event(info, clear_state, _, _) ->
    {keep_state, #state{}, []};
handle_event({call, From}, Content, StateName, State) ->
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, [{reply, From, [{text, <<"Tenhle příkaz teď nejde použít"/utf8>>}]}]}.

%% private functions

get_assignment(finish, #game{}) ->
    [{text, <<"Gratulujeme k absolvování hry"/utf8>>}];
get_assignment({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{assignment = Assignment}} = Moves,
    Assignment;
get_assignment({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{assignment = Assignment}} = Puzzles,
    Assignment.

get_answer({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{answer = Answer}} = Moves,
    Answer;
get_answer({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{answer = Answer}} = Puzzles,
    Answer.

get_next_state({move, MoveName}, #game{moves = Moves}) ->
    #{MoveName := #task{next_state = NextState}} = Moves,
    NextState;
get_next_state({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{next_state = NextState}} = Puzzles,
    NextState.

get_hint({puzzle, PuzzleName}, #game{puzzles = Puzzles}) ->
    #{PuzzleName := #task{hint = Hint}} = Puzzles,
    Hint.

get_next_assignment(CurrentState, Game) ->
    NextState = get_next_state(CurrentState, Game),
    Assignment = get_assignment(NextState, Game),
    {NextState, Assignment}.

minutes_seconds(Seconds) ->
    {Seconds div 60, Seconds rem 60}.

%% score:

log(Event, StateName, #state{game_log = Log, time0 = Time0} = State) ->
    EventTime = erlang:monotonic_time() - Time0,
    State#state{game_log = [{EventTime, Event, StateName} | Log]}.

hint_used(CurrentState, #state{game_log = GameLog}) ->
    length(lists:filter(fun({_, hint, CS}) when CS =:= CurrentState -> true;
                           (_) -> false end,
                        GameLog)) > 0.

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
