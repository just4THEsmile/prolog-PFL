
%These lines are to load the necessary files to the execution of our game
:- consult(config).
:- consult(utils).
:- consult(data).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

%The play/0 starts the Game
play:-
    configurations(Gamestate),!,
    true.



    
