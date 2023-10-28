:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

display_board([Board,_,_]):-
    clear,
    print_board(Board).
    

init_state(Board):-
    board(Board).

print_board(Board):-
    length(Board, NumRows),
    print_board_rows(Board, NumRows).

print_board_rows(_, 0).
print_board_rows(Board, NumRows):-
    length(Board, BoardLength),
    NumSpaces is abs(NumRows - (BoardLength + 1) // 2),
    print_spaces(NumSpaces),
    nth1(NumRows, Board, Row),
    write_row(Row),
    nl,
    NextNumRows is NumRows - 1,
    print_board_rows(Board, NextNumRows).

print_spaces(0).
print_spaces(N):-
    write(' '),
    NextN is N - 1,
    print_spaces(NextN).

write_row([]).
write_row([H|T]):-
    write_hexagon_tile(H),
    write(' '),
    write_row(T).

write_hexagon_tile(empty):-
    write('O').
write_hexagon_tile(white):-
    write('X').
write_hexagon_tile(black):-
    write('*').