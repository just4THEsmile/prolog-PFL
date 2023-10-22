
%These lines are to load the necessary files to the execution of our game
:- consult(config).
:- consult(utils).
:- use_module(library(between)).

%The play/0 starts the Game
play:-
    drawMenuPlayer, %The drawMenuPlayer/0 draws the menu of the game
    gamemode(Gamemode), write(Gamemode), nl, 
    true.

    
