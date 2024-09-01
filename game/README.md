game
=====

An OTP application

Build
-----

    $ rebar3 compile

Flow
====

* A game-fsm process is started by `game_fsm:start_link(GameSpecPath)` where the argument is
a file path to definition of a game
    - the file from GameSpecPath is resolved and the file is parsed by game_records:game_from_json/1
    - checks whether all games states point to another existing state are run
* Multiple events are sent to game-fsm proceses via `game_fsm:send_event(Pid, Message)` where
Pid is a pid of specific game_fsm process and Message is (currently) a binary string. Message is parsed and evaluated. Output is a list of responses for that event
