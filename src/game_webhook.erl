-module(game_webhook).
-export([out/1]).
-include_lib("yaws/include/yaws_api.hrl").
out(#arg{req = Req, clidata = Clidata} = Arg) ->
    %{abs_path, AbsPath} = (Arg#arg.req)#http_request.path,
    [{status, 200},
     {content, "text/plain", io_lib:format("~p~n", [Clidata])}].
