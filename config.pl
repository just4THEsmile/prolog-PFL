:- consult(data).

% configuration(-GameState)
% Initialize GameState with Board and Player Turn
configurations([Board,Player]):-
    drawStartMenu,
    getGamemode(Gamemode),
    !,
    option(Gamemode),
    startPlayer(Player),
    getBoardSize(Size),
    init_state(Size,Board).


drawStartMenu:-
    clear,

    write('=====================================================================================\n'),
    write('|                           Hello and Welcome to DuAlma Game.                       |\n'),
    write('| Select your desired gamemode:                                                     |\n'),
    write('|     1. Human vs Human                                                             |\n'),
    write('|     2. Human vs Bot                                                               |\n'),
    write('|     3. Bot vs Bot                                                                 |\n'),
    write('=====================================================================================\n'),
    write(' Please Insert a Number, with a dot in the end, to select the gamemode:').
drawBotDifficultyMenu(BotName):-
    clear,
    write('=====================================================================================\n'),
    format('|Select the difficulty of the ~a:                                                  |\n',[BotName]),
    write('|     1. Easy                                                                       |\n'),
    write('|     2. Hard                                                                       |\n'),
    write('| Please Insert a Number, with a dot in the end, to select the difficulty.          |\n'),
    write('=====================================================================================\n').


% option(+N)
% Main menu options. Each represents a game mode.
option(1):-
    write('\nSelected Gamemode was Human vs Human\n\n'),
    getPlayerName(player1),
    welcomePlayer(player1),
    getPlayerName(player2),
    welcomePlayer(player2).

option(2):-
    write('\nSelected Gamemode was Human vs Bot\n\n'),
    getPlayerName(player1),
    welcomePlayer(player1),
    asserta((name_of_the_player(player2,'Bot'))), !,
    drawBotDifficultyMenu('Bot'),
    getBotDifficulty(player2).

option(3):-
    write('\nSelected Gamemode was Bot vs Bot\n\n'),
    asserta(name_of_the_player(player1,'FitBot')), !,
    asserta(name_of_the_player(player2,'FatBot')), !,
    drawBotDifficultyMenu('FitBot'),
    getBotDifficulty(player1),
    drawBotDifficultyMenu('FatBot'),
    getBotDifficulty(player2).
    
    
welcomePlayer(Player):-
    name_of_the_player(Player, Name),
    format('\nWelcome to the game, ~a!', [Name]), nl.

startPlayer(Player):-
    name_of_the_player(player1, Name1),
    name_of_the_player(player2, Name2),
    format('\nWho will start the game?Select 1 for ~a or select 2 for ~a?', [Name1,Name2]), nl,
    getStartPlayer(Player).
    


    
