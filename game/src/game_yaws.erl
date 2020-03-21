-module(game_yaws).
-compile(export_all).

start() ->
    {ok, spawn_link(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    SconfList = [{port, 8888},
                 {servername, "foobar"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
                 {appmods, [{"/", game_webhook}]}
                ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(game_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    exit(normal).
