-module(game_fsm).
-behaviour(gen_statem).
-export([init/1,
         callback_mode/0,
         handle_event/4]).
-export([start/1,
         send_event/2]).
-include("game.hrl").

-record(state, {
            game,
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

handle_event({call, From}, <<"reseni 123">>, {move, MoveName}, #state{} = State) ->
    {next_state, {puzzle, <<"rozsypany_caj">>}, State, [{reply, From, <<"move ok: to bylo dobre">>}]};
handle_event({call, From}, Event, {move, MoveName}, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"jeste tam nejsi">>}]};

handle_event({call, From}, <<"reseni vino">>, {puzzle, PuzzleName}, #state{} = State) ->
    {next_state, {move, <<"k_cinaku">>}, State, [{reply, From, <<"puzzle ok: to bylo dobre">>}]};
handle_event({call, From}, Event, {puzzle, MoveName}, #state{} = State) ->
    {keep_state, State, [{reply, From, <<"to neni spravny kod">>}]};
handle_event(Type, Content, StateName, State) ->
    io:format("Type: ~p~n", [Type]),
    io:format("Content: ~p~n", [Content]),
    io:format("StateName: ~p~n", [StateName]),
    io:format("State: ~p~n", [State]),
    {keep_state, State, []}.
