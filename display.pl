:- consult(data).
:- consult(utils).

% display_game(+GameState)
% Displays the board.
display_game([Board,_]):-
    length(Board, BoardLength),
    True_BoardLength is (BoardLength + 1) ,
    print_board_rows(Board,1,True_BoardLength), nl,
    true.

% init_state(+Size, -Board)
% Initializes the board with the given size.  
init_state(Size,Board):-
    board(Size,Board).

% print_board_rows(+Board, +NumRows, +MaxRows)
% Prints the board rows.
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

% print_spaces(+N)
% Prints N spaces.
print_spaces(0).
print_spaces(N):-
    N > 0,
    write(' '), 
    NextN is N - 1,
    print_spaces(NextN).

% write_row(+Row)
% Writes a row of the board.
write_row([]).
write_row([H|T]):-
    write_hexagon_tile(H),
    write('  '),
    write_row(T).

% write_hexagon_tile(+Tile)
% Writes a tile of the board according to the atom.
write_hexagon_tile(empty):-
    put_code(0x25CB).
write_hexagon_tile(white):-
    put_code(0x25CE).
write_hexagon_tile(black):-
    put_code(0x25C9).
write_hexagon_tile(whiteblack):-
    put_code(0x25D2).
write_hexagon_tile(blackwhite):-
    put_code(0x25D3).
write_hexagon_tile(whitewhite):-
    put_code(0x25FB).
write_hexagon_tile(blackblack):-
    put_code(0x25FC).

% print_stats(+GameState)
% Prints the player which plays the turn.
print_stats([_,Player]):-
    (Player = 1 ->
        name_of_the_player(player1, Name)
    ; Player = 2 ->
        name_of_the_player(player2, Name)
    ),
    format('It\'s Player ~a\'s turn. Good luck!', [Name]),
    true.


winner_screen(Gamestate,Winner):-
    clear,
    (Winner =:= 1 -> name_of_the_player(player1, Name) ; name_of_the_player(player2, Name)),
    format('Player ~a won the game!', [Name]), nl,
    display_game(Gamestate).