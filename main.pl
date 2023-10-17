
%These lines are to load the necessary files to the execution of our game
:- consult(config).
:- use_module(library(between)).
%The play/0 starts the Game
play:-
    drawMenuPlayer,
    
    drawMenuDifficulty.
