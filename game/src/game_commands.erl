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
Vyluštil-li ji zadání a chceš ho zadat, napiš 'odpověď CO_TI_VYŠLO' např: 'odpověď mrkev'.
Chceš-li zopakovat zadání, napiš 'zadání' nebo 'zopakuj zadání'.
Nevíš-li si rady se zadáním a chceš-li nápovědu, napiš 'nápověda'.
Jsi-li zoufalý a chceš-li vzdát luštění této šifry a jít dál, napiš 'řešení'.
Jsi-li jen společenský, napiš 'ahoj'.
"/utf8>>.

make_command([<<"?">>]) ->
    {help};
make_command([<<"help">>]) ->
    {help};
make_command([<<"pomoc">>]) ->
    {help};

make_command([<<"ahoj">>]) ->
    {hello};

make_command([<<"stav">>]) ->
    {score};
make_command([<<"jak">>, <<"jsem">>, <<"na">>, <<"tom">>]) ->
    {score};

make_command([<<"odpoved">>, Guess]) ->
    {guess, Guess};
make_command([<<"odpověď"/utf8>>, Guess]) ->
    {guess, Guess};
make_command([<<"odpoved">>, <<"je">>, Guess]) ->
    {guess, Guess};
make_command([<<"odpověď"/utf8>>, <<"je">>, Guess]) ->
    {guess, Guess};

make_command([<<"napoveda">>]) ->
    {hint};
make_command([<<"nápověda"/utf8>>]) ->
    {hint};

make_command([<<"zadani">>]) ->
    {assignment};
make_command([<<"zopakuj">>, <<"zadani">>]) ->
    {assignment};
make_command([<<"zadání"/utf8>>]) ->
    {assignment};
make_command([<<"zopakuj">>, <<"zadání"/utf8>>]) ->
    {assignment};

make_command([<<"reseni">>]) ->
    {give_up};
make_command([<<"řešení"/utf8>>]) ->
    {give_up};

make_command([<<"ano">>]) ->
    {yes};
make_command([<<"ne">>]) ->
    {no};

make_command(_) ->
    error.
