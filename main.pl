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

% play
% The play/0 starts the Game
play:-
    configurations(Gamestate),!,
    initialize_player_pieces,
    game_cycle_first_phase(Gamestate),
    %game_cycle_second_phase(Gamestate),
    write('over'),
    true.


% initialize_player_pieces
% The initialize_player_pieces/0 initialize the number of pieces of each color
initialize_player_pieces:-
    asserta(disks(11,11)).
