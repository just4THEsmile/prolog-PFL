:- use_module(library(lists)).
:- consult(data).
:- consult(utils).


display_board([Board,_]):-
    %clear,
    length(Board, BoardLength),
    True_BoardLength is (BoardLength + 1) ,
    print_board_rows(Board,1,True_BoardLength),
    true.
    
init_state(Size,Board):-
    board(Size,Board).


print_board_rows(_, NumRows, NumRows):-!.
print_board_rows(Board, NumRows, MaxRows):-
    length(Board, BoardLength),
    NumSpaces is abs(NumRows - (BoardLength + 1) // 2),
    NumSpaces1 is NumSpaces*2,
    nl,
    print_spaces(NumSpaces1),
    nth1(NumRows, Board, Row),
    write_row(Row),
    nl,
    NextNumRows is NumRows + 1,
    print_board_rows(Board, NextNumRows, MaxRows).

print_spaces(0).
print_spaces(N):-
    N > 0,
    write(' '), 
    NextN is N - 1,
    print_spaces(NextN).

write_row([]).
write_row([H|T]):-
    write_hexagon_tile(H),
    write('  '),
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


print_stats([_,Player]):-
    (Player = 1 ->
        name_of_the_player(player1, Name)
    ; Player = 2 ->
        name_of_the_player(player2, Name)
    ),
    format('It\'s Player ~a\'s turn. Good luck!', [Name]),
    true.