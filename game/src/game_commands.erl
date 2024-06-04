-module(game_commands).
-export([command/1]).
-compile([export_all]).

command(Binary) when is_binary(Binary) ->
    ComparableString = string:casefold(Binary),
    Tokens = string:lexemes(ComparableString, " " ++ [$\n, $\t, $\r]),
    make_command(Tokens).


help() ->
    <<"Použití:
Příklady zde uvedené zadávej tak, jak jsou psány, tj.většinou bez tečky či otazníku na konci.
Můžeš psát s diakritikou i bez.
      
Nevíš-li, jaké máš možnosti napiš '?' nebo 'pomoc'.
Chceš-li vědět, jak jsi na tom, napiš 'stav' nebo 'jak jsem na tom'.
Vyluštil-li ji zadání a chceš ho zadat, napiš 'heslo CO_TI_VYŠLO' např: 'heslo mrkev'.
Chceš-li zopakovat zadání, napiš 'zadání' nebo 'zopakuj zadání'.
Nevíš-li si rady se zadáním a chceš-li nápovědu, napiš 'nápověda'.
Jsi-li zoufalý a chceš-li vzdát luštění této šifry a jít dál, napiš 'řešení'.
Jsi-li jen společenská, napiš 'ahoj'.
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

make_command([<<"reseni"/utf8>>]) ->
    {give_up};
make_command([<<"řešení"/utf8>>]) ->
    {give_up};

make_command([<<"ano"/utf8>>]) ->
    {yes};
make_command([<<"ne"/utf8>>]) ->
    {no};

make_command(_) ->
    error.
