-module(game_commands).
-export([command/1]).
-compile([export_all]).

command(Binary) when is_binary(Binary) ->
    ComparableString = string:casefold(Binary),
    Tokens = string:lexemes(ComparableString, " " ++ [$\n, $\t, $\r]),
    make_command(Tokens).


help() ->
    <<"Pro komunikaci s programem používej následující pokyny:
      
'?' nebo 'pomoc' Nevíš-li, jak si se mnou povídat.

'stav' Chceš-li vědět, jak jsi na tom.

'heslo mrkev' Vyluštila-li jsi šifru a vyšla ti mrkev.

'zadání' nebo 'zopakuj zadání' Chceš-li zopakovat zadání.

'nápověda' Nevíš-li si rady se šifrou a chceš nápovědu.

'ano' nebo 'ne' Pokud se tě program na něco ptá.

'přeskočit' Jsi-li zoufalá a chceš vzdát luštění této šifry a jít dál.

'ahoj' Jsi-li jen společenská.
"/utf8>>.

make_command([<<"?"/utf8>>]) ->
    {help};
make_command([<<"help"/utf8>>]) ->
    {help};
make_command([<<"pomoc"/utf8>>]) ->
    {help};

make_command([<<"ahoj"/utf8>>]) ->
    {hello};

make_command([<<"stav"/utf8>>]) ->
    {score};
make_command([<<"jak"/utf8>>, <<"jsem"/utf8>>, <<"na"/utf8>>, <<"tom"/utf8>> | _]) ->
    {score};

make_command([<<"heslo"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"heslo"/utf8>>, <<"je"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"odpoved"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"odpověď"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"odpoved"/utf8>>, <<"je"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"odpověď"/utf8>>, <<"je"/utf8>>, Guess]) ->
    {guess, Guess};

make_command([<<"napoveda"/utf8>>]) ->
    {hint};
make_command([<<"nápověda"/utf8>>]) ->
    {hint};

make_command([<<"zadani"/utf8>>]) ->
    {assignment};
make_command([<<"zopakuj"/utf8>>, <<"zadani"/utf8>>]) ->
    {assignment};
make_command([<<"zadání"/utf8>>]) ->
    {assignment};
make_command([<<"zopakuj"/utf8>>, <<"zadání"/utf8>>]) ->
    {assignment};

make_command([<<"přeskočit"/utf8>>]) ->
    {give_up};
make_command([<<"preskocit"/utf8>>]) ->
    {give_up};
make_command([<<"přeskoč"/utf8>>]) ->
    {give_up};
make_command([<<"preskoc"/utf8>>]) ->
    {give_up};

make_command([<<"ano"/utf8>>]) ->
    {yes};
make_command([<<"ne"/utf8>>]) ->
    {no};

make_command(_) ->
    error.
