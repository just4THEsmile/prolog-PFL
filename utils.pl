:-consult(data).

clear_input:-
    repeat,
    get_char(C),
    C = '\n'.

clear:- 
    write('\33\[2J').

getLine(Result, Acc) :-
    get_char(Char),
    Char \= '.',
    Char \= '\n',
    append(Acc, [Char], Acc1),
    getLine(Result, Acc1).
getLine(Result, Acc) :-
    atom_chars(Result, Acc).

getPlayerName(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    getLine(Name, []),
    asserta(name_of_the_player(Player, Name)),
    clear_input.

getGamemode(Gamemode):-
    repeat,
    read(Gamemode),
    between(1,3,Gamemode),
    clear_input,
    !.

getBotDifficulty(Bot):-
    repeat,
    read(BotDifficulty),
    between(1,2,BotDifficulty),
    asserta(bot_difficulty(Bot, BotDifficulty)),
    clear_input.

getStartPlayer(StartPlayer):-
    repeat,
    read(StartPlayer),
    between(1,2,StartPlayer),
    clear_input.