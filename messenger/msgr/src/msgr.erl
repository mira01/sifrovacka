-module(msgr).
-compile([export_all]).
-behaviour(gen_server).

-record(state, {
                access_token :: binary()
               ,endpoint :: binary()
               }
       ).
-record(request, {
          method :: get | post | put | delete,
          url :: string(),
          headers = [] :: [{string(), string()}],
          content_type :: string(),
          body :: string() | binary(),
          http_options = [] :: [any()],
          client_options = [] :: [any()]
         }).

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

%% Private functions

send_sync(Recipient, Message, Context) ->
    Url = build_url(Context),
    Request = build_request(Recipient, Message),
    io:format("Request headers: ~p~n", [Request#request.headers]),
    io:format("Request body: ~s~n", [Request#request.body]),
    send_request(Request#request{url = Url}).

send_request(#request{
                method = Method,
                url = Url,
                headers = Headers,
                content_type = ContentType,
                body = Body,
                http_options = HttpOptions,
                client_options = ClientOptions
               }) ->
    httpc:request(Method, {Url, Headers, ContentType, Body}, HttpOptions, [{sync, true} | ClientOptions]).

build_url(#state{endpoint = Endpoint, access_token = AccessToken}) ->
    Endpoint ++ "?access_token=" ++ AccessToken.

file_metadata({Type, BinPath}) ->
    Path = binary_to_list(BinPath),
    Filename = filename:basename(Path),
    [$. | Extension] = filename:extension(Filename),
    Mime = mime(Type, string:lowercase(Extension)),
    {Filename, Mime}.

mime(image, "jpg") ->
    mime(image, "jpeg");
mime(Type, Extension) ->
    LowerExtension = string:lowercase(Extension),
    StartBoundary = erlang:iolist_to_binary([atom_to_list(Type), "/", LowerExtension]).


build_request(Recipient, {text, Text}) ->
    #request{
       method = post,
       content_type = "application/json",
       body = jiffy:encode(
                #{
                <<"recipient">> => #{<<"id">> => Recipient},
                <<"message">> => #{<<"text">> => Text}
               })
      };

build_request(Recipient, {question, Text}) ->
    #request{
       method = post,
       content_type = "application/json",
       body = jiffy:encode(
                #{
                <<"recipient">> => #{<<"id">> => Recipient},
                <<"message">> => #{<<"text">> => Text}
               })
      };

build_request(Recipient, {Asset, Path} = Message) when is_atom(Asset) ->
    {Filename, Mime} = file_metadata(Message),
    {ok, Binary} = file:read_file(Path),

    TextParts = [
                 {<<"recipient">>, jiffy:encode(#{<<"id">> => Recipient})}
                 ,{<<"message">>, jiffy:encode(#{<<"attachment">> =>
                                                 #{<<"type">> => list_to_binary(atom_to_list(Asset)),
                                                   <<"payload">> => #{<<"is_reusable">> => true}
                                                  }
                                                }
                                              )
                  }
                ],
    BinaryParts = [{<<"filedata">>, Binary, Mime, Filename}],

    Boundary = <<"http multipart boundary string">>,
    Body = format_multipart(TextParts, BinaryParts, Boundary),
    ContentLength = integer_to_list(length(binary_to_list(Body))),
    #request{
        method = post,
        headers = [{"Content-Length", ContentLength}],
        content_type = "multipart/form-data; boundary=" ++ binary_to_list(Boundary),
        body = Body,
        client_options = [{body_format, binary}]
    }.

format_multipart(TextData, BinaryData, Boundary) ->
    StartBoundary = erlang:iolist_to_binary([<<"--">>, Boundary]),
    LineSeparator = <<"\r\n">>,
    TextParts = lists:foldl(fun({Key, Value}, Acc) ->
        erlang:iolist_to_binary([
            Acc,
            StartBoundary, LineSeparator,
            <<"Content-Disposition: form-data; name=\"">>, Key, <<"\"">>, LineSeparator, LineSeparator,
            Value, LineSeparator
        ])
    end, <<"">>, TextData),
    AllParts = lists:foldl(fun({Key, Binary, Mime, Filename}, Acc) ->
        erlang:iolist_to_binary([
            Acc,
            StartBoundary, LineSeparator,
            <<"Content-Disposition: form-data; name=\"">>, Key, <<"\"; filename=\"">>, Filename, <<"\"">>, LineSeparator,
            <<"Content-Type: ">>, Mime, LineSeparator, LineSeparator,
            Binary,
            LineSeparator
        ])
    end, TextParts, BinaryData),
    erlang:iolist_to_binary([AllParts, StartBoundary, <<"--">>, LineSeparator]).
