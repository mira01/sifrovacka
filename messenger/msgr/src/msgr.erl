-module(msgr).
-compile([export_all]).
-behaviour(gen_server).

-record(state, {
                access_token :: binary()
               ,endpoint :: binary()
               }
       ).

%% API

start_link(AccessToken, Endpoint) ->
    gen_server:start(?MODULE, [AccessToken, Endpoint], []).

send(Sender, Recipient, Message) ->
    gen_server:call(Sender, {send, Recipient, Message}).

%% Callbacks

init([AccessToken, Endpoint]) ->
    {ok, #state{access_token = AccessToken, endpoint = Endpoint}}.

handle_call({send, Recipient, Message}, _From, State) ->
    HttpResp = send_sync(Recipient, Message, State),
    {ok, {{_, Status, _}, _Headers, _Body} = ResponseContent} = HttpResp,
    Resp = case Status of
        200 ->
            HttpResp;
        _ ->
            {error, ResponseContent}
    end,
    {reply, Resp, State}.

send_sync(Recipient, Message, Context) ->
    {Method, Request} = build_request(Recipient, Message, Context),
    httpc:request(Method, Request, [], [{sync, true}]).

build_request(Recipient, Message, Context) ->
    {
     post
     ,{
       Context#state.endpoint ++ "?access_token=" ++ Context#state.access_token
       ,[]
       ,"application/json"
       ,jiffy:encode(response(Recipient, Message))
      }
    }.
response(Recipient, Message) ->
    #{
      <<"recipient">> => #{<<"id">> => Recipient},
      <<"message">> => message(Message)
     }.

message({text, Text}) when is_binary(Text) ->
    #{<<"text">> => Text}.

