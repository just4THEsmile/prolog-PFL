:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

display_board([Board,_,_]):-
    clear,
    print_board(Board).
    

init_state(Size,Board):-
    board(Size,Board).

print_board(Board):-
    length(Board, NumRows),
    print_board_rows(Board, 1, NumRows).

print_board_rows(_, NumRows, NumRows):- !.
print_board_rows(Board, NumRows, MaxRows):-
    length(Board, BoardLength),
    NumSpaces is abs(NumRows - (BoardLength + 1) // 2),
    NumSpaces1 is NumSpaces + 55,
    print_spaces(NumSpaces1),
    nth1(NumRows, Board, Row),
    write_row(Row),
    nl,
    NextNumRows is NumRows + 1,
    print_board_rows(Board, NextNumRows, MaxRows).

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
    put_code(9711).
write_hexagon_tile(white):-
    write('w').
write_hexagon_tile(black):-
    write('b').
write_hexagon_tile(whiteblack):-
    write('g').
write_hexagon_tile(blackwhite):-
    write('G').
write_hexagon_tile(whitewhite):-
    write('W').
write_hexagon_tile(blackblack):-
    write('B').


print_stats([_,Player,_]):-
    name_of_the_player( Player, Name ),
    format('It's Player ~a turn. Good luck!', [Name]).