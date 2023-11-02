%These lines are to load the necessary files to the execution of our game
:- consult(config).
:- consult(utils).
:- consult(data).
:- consult(display).
:- consult(logic).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

%The play/0 starts the Game
play:-
    configurations(Gamestate),!,
    initialize_player_pieces,
    game_cycle_first_phase(Gamestate,NewGamestate),
    write('over'),
    game_cycle_second_phase(NewGamestate),
    write('over'),
    true.



initialize_player_pieces:-
    asserta(disks(2,2)).
