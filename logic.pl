:-consult(data).
:-consult(display).


game_cycle(Gamestate):-
    display_board(Gamestate), 
    true.



/*
print_stats(Gamestate),
choose_move(Gamestate, Move),
move(Gamestate, Move, NewGamestate),
!,
game_cycle(NewGamestate).
*/