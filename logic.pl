:-consult(data).
:-consult(display).

game_cycle_first_phase(Gamestate):-
    first_phase_over(Gamestate).

game_cycle_first_phase(Gamestate):-
    display_board(Gamestate), 
    print_stats(Gamestate),
    put_piece_input(Gamestate, Put),
    put_piece_move( Gamestate, Put, NewGamestate),
    game_cycle_first_phase(NewGamestate).


game_cycle_second_phase(Gamestate):-
    game_over(Gamestate, Winner),
    !,
    display_end(Gamestate, Winner).

game_cycle_second_phase(Gamestate):-
    display_board(Gamestate), 
    print_stats(Gamestate),
    move_input(Gamestate, Move),
    move(Gamestate, Move, NewGamestate),
    game_cycle_second_phase(NewGamestate).

/*
print_stats(Gamestate),
choose_move(Gamestate, Move),
move(Gamestate, Move, NewGamestate),
!,
game_cycle(NewGamestate).
*/