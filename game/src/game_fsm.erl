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
          time0 = undefined,
          finish_time = undefined,
          game_log = [] :: [any()]
         }).

-define(POINTS_PUZZLE_OK, 10).
-define(POINTS_HINT, -5).

%   {ok, SessionHolder} = game_session_sup:sessions(<<"cibulka"/utf8>>).
%   game_session:get_session(SessionHolder, mirek).


%   f(Pid), {ok, Pid} = game_fsm:start_link("/definitions/cibulka_game/definition.json").
%   game_fsm:send_event(Pid, <<"napoveda"/utf8>>).
%
%   f(B), f(All), [{_, B} | _] = All = game_fsm:send_event(Pid, <<"ano"/utf8>>), io:format("~ts", [B]).

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
    {ok, Start, #state{game = Game, time0 = undefined}}.

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

handle_event({call, From}, {score}, _, #state{time0 = undefined, game_log = GameLog} = State) ->
    Points = score(State),
    {keep_state, State, [{reply, From, [
        {text, unicode:characters_to_binary(io_lib:format("Máš ~p bodů a čas jsme zatím nespustili", [Points]))}
                                       ]}]};
handle_event({call, From}, {score}, CurrentState, #state{time0 = StartTime, game_log = GameLog} = State) when CurrentState =/= finish->
    DurationSec = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, second),
    {Hours, Minutes, Seconds} = hours_minutes_seconds(DurationSec),
    Points = score(State),
    {keep_state, State, [{reply, From, [
        {text, unicode:characters_to_binary(io_lib:format("Máš ~p bodů a hraješ ~p hodin ~p minut a ~p vteřin", [Points, Hours, Minutes, Seconds]))}
                                       ]}]};


handle_event({call, From}, {assignment}, {move, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};
handle_event({call, From}, {assignment}, {puzzle, _} = CurrentState, #state{game = Game} = State) ->
    Assignment = get_assignment(CurrentState, Game),
    {keep_state, State, [{reply, From, Assignment}]};

handle_event({call, From}, {guess, Guess}, CurrentState, #state{game = Game} = State) when CurrentState =/= finish->
    Answer = get_answer(CurrentState, Game),
    case Guess =:= Answer of
        true ->
            {NextState, State2, Replies} = on_correct_answer(CurrentState, State),
            {next_state, NextState, State2, [{reply, From, Replies}]};
        false ->
            State2 = log({answer_wrong, Guess}, CurrentState, State),
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
    R = [{text, <<"Nevadí, jedeme dál..."/utf8>>} | NextAssignment],
    {next_state, NextState, State2, [{reply, From, R}]};

handle_event({call, From}, {no}, {confirm_giveup, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"No už jsem se lekl. Pojď, to dáš."/utf8>>}]}]};

handle_event({call, From}, _, {confirm_giveup, StateToGo}, State) ->
    {next_state, StateToGo, State, [{reply, From, [{text, <<"Neřekla jsi, jestli opravdu chceš přeskočit úkol. Tak jsem raději neudělal nic"/utf8>>}]}]};

handle_event({call, From}, _Event, finish, #state{time0 = StartTime, finish_time = FinishTime} = State) ->
    DurationSec = erlang:convert_time_unit(FinishTime - StartTime, native, second),
    {Hours, Minutes, Seconds} = hours_minutes_seconds(DurationSec),
    Points = score(State),
    {keep_state, State, [{reply, From, [
        {text, unicode:characters_to_binary(io_lib:format("Došla jsi do cíle s ~p body za ~p hodin ~p minut a ~p vteřin. Velká gratulace", [Points, Hours, Minutes, Seconds]))}
                                       ]}]};

handle_event({call, From}, {hello}, CurrentState, #state{game = #game{welcome = Welcome} = Game} = State) ->
    {keep_state, State, [{reply, From, Welcome }]};

handle_event(info, clear_state, _, _) ->
    {keep_state, #state{}, []};

handle_event({call, From}, Content, StateName, State) ->
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, [{reply, From, [{text, <<"Tenhle příkaz teď nejde použít"/utf8>>}]}]}.

%% private functions

on_correct_answer({StateType, _StateName} = CurrentState, #state{game = Game} = State) ->
    {NextState, NextAssignment} = get_next_assignment(CurrentState, Game),
    State2 = log(answer_ok, CurrentState, State),
    Replies = [{text, <<"Výborně, dobrá práce!"/utf8>>}] ++ NextAssignment,
    {TimerJustStarted, State3} = start_timer_if_needed(CurrentState, State2),
    Replies2 = case TimerJustStarted of
                   true -> Replies ++ [{text, <<"A spustili jsme čas"/utf8>>}];
                   _ -> Replies
               end,
    State4 = case NextState of
                 finish -> State3#state{finish_time = erlang:monotonic_time()};
                 _ -> State3
             end,
    {NextState, State4, Replies2}.


get_assignment(finish, #game{bye = Bye}) ->
    Bye;
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

start_timer_if_needed(CurrentState, #state{game = #game{time_starts_after = StartingState}} = State) ->
    case CurrentState =:= StartingState of
        true -> {true, State#state{time0 = erlang:monotonic_time()}};
        _ -> {false, State}
    end.

hours_minutes_seconds(Seconds) ->
    {Hours, Rem1} = {Seconds div 3600, Seconds rem 3600},
    {Hours, Rem1 div 60, Rem1 rem 60}.

%% score:

log(Event, StateName, #state{game_log = Log, time0 = undefined} = State) ->
    State#state{game_log = [{before_time_started, Event, StateName} | Log]};
log(Event, StateName, #state{game_log = Log, time0 = Time0} = State) ->
    EventTime = erlang:monotonic_time() - Time0,
    State#state{game_log = [{EventTime, Event, StateName} | Log]}.

hint_used(CurrentState, #state{game_log = GameLog}) ->
    length(lists:filter(fun({_, hint, CS}) when CS =:= CurrentState -> true;
                           (_) -> false end,
                        GameLog)) > 0.

score(#state{game_log = GameLog}) ->
    PuzzleOKs = [PuzzleName || {_, answer_ok, {puzzle, PuzzleName}} <- GameLog],
    io:format("PuzzleOKs: ~p~n", [PuzzleOKs]),
    HintsUsed = [PuzzleName || {_, hint, {_, PuzzleName}} <- GameLog],
    io:format("HintsUsed: ~p~n", [HintsUsed]),
    HintsForOKPuzzle = [PuzzleState || PuzzleState <- HintsUsed, lists:member(PuzzleState, PuzzleOKs)],
    io:format("HintsForOKPuzzle: ~p~n", [HintsForOKPuzzle]),
    %TODO: rozepsat vysledky
    length(PuzzleOKs) * ?POINTS_PUZZLE_OK
    + length(HintsForOKPuzzle) * ?POINTS_HINT.
