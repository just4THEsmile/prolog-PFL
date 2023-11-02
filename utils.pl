:-consult(data).

% clear_input
% clears the input buffer
clear_input:-
    repeat,
    get_char(C),
    C = '\n'.

% clear
% clears the screen
clear:- 
    write('\33\[2J').


% getLine(-Result, +Acc)
% Reads a line from the input and returns it in Result
getLine(Result, Acc) :-
    get_char(Char),
    Char \= '.',
    Char \= '\n',
    append(Acc, [Char], Acc1),
    getLine(Result, Acc1).
getLine(Result, Acc) :-
    atom_chars(Result, Acc).


% getPlayerName(-Player)
% Asks the player for his name and returns it in Player
getPlayerName(Player):-
    format('\nHello ~a, what is your name? ', [Player]),
    getLine(Name, []),
    asserta(name_of_the_player(Player, Name)),
    clear_input.

% getGameMode(-GameMode)
% Asks the player for the game mode and returns it in GameMode
getGamemode(Gamemode):-
    repeat,
    read(Gamemode),
    between(1,3,Gamemode),
    clear_input,
    !.

% getBotDifficulty(+Bot)
% Asks the player for the bot difficulty and applies it to Bot
getBotDifficulty(Bot):-
    repeat,
    read(BotDifficulty),
    between(1,2,BotDifficulty),
    asserta(bot_difficulty(Bot, BotDifficulty)),
    clear_input.

% getStartPlayer(-StartPlayer)
% Asks the player for the start player and returns it in StartPlayer
getStartPlayer(StartPlayer):-
    repeat,
    read(StartPlayer),
    between(1,2,StartPlayer),
    asserta(white(StartPlayer)),
    (StartPlayer =:= 1 -> asserta(black(2)); asserta(black(1))),
    clear_input.

% getBoardSize(-Size)
% Asks the player for the board size and returns it in Size
getBoardSize(Size):-
    repeat,
    write('\nChoose the size of the board (5 or 6):'),
    read(Size),
    member(Size, [5,6]),
    clear_input.


% manhattan_distance(+Position1, +Position2, -Distance)
% Calculates the manhattan distance between two positions
manhattan_distance([Row1, Column1], [Row2, Column2], Distance):-
    Distance is abs(Row1 - Row2) + abs(Column1 - Column2).

% sum_list(+List, -Sum)
% Sums all the elements of a list
sum_list([], 0).
sum_list([X|Xs], Sum):-
    sum_list(Xs, RestSum),
    Sum is X + RestSum.