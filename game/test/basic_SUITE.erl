-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0]).
-export([start/1]).

all() -> [start].

%% @doc Test if we can start a player's session and it replies to events
start(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    GameDefinition = DataDir ++ "definition.json",

    {ok, Pid} = game_fsm:start_link(GameDefinition),
    Responses = game_fsm:send_event(Pid, <<"napoveda"/utf8>>),

    ?assertMatch([{question, _}], Responses).

