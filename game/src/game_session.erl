-module(game_session).
-compile([export_all]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2]).
-export([get_session/2, revoke_session/2]).

-behaviour(gen_server).

-record(state, {
                name
               ,game_path
               ,sessions
               }
       ).

start_link(Name, Path) ->
    gen_server:start_link(?MODULE, [Name, Path], []).

get_session(Pid, Sid) ->
    gen_server:call(Pid, {get_session, Sid}).

revoke_session(Pid, Sid) ->
    gen_server:cast(Pid, {session_down, Sid}).

%%%%

init([Name, Path]) ->
    process_flag(trap_exit, true),
    {ok, #state{name=Name, game_path=Path, sessions=dict:new()}}.

handle_call({get_session, Sid}, _From, #state{game_path=Path, sessions=Sessions} = State) ->
    {ok, Pid} = case dict:find(Sid, Sessions) of
                         error ->
                             game_fsm:start_link(Path);
                         {ok, {APid, _LastAccessed}} ->
                            case is_process_alive(APid) of
                                true -> {ok, APid};
                                false -> game_fsm:start_link(Path)
                            end
                     end,
    NewState = State#state{sessions = dict:store(Sid, {Pid, erlang:system_time()}, Sessions)},
    {reply, Pid, NewState};

handle_call(Msg, _From, State) ->
    {reply, {got, Msg}, State}.

handle_info({'EXIT', Pid, Reason}, #state{sessions = Sessions} = State) ->
    io:format("Exit ~p because of: ~p~n", [Pid, Reason]),
    NewSessions = dict:filter(fun(Sid, {APid, _}) when APid == Pid -> true;
                   (_, _) -> false
                end, Sessions),
    {noreply, State#state{sessions=NewSessions}}.

handle_cast({session_down, Sid}, State) ->
    {noreply, dict:erase(Sid, State)}.
