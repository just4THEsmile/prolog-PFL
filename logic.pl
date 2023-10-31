:-consult(data).
:-consult(display).

/*game_cycle_first_phase(Gamestate):-
    first_phase_over(Gamestate).
*/
game_cycle_first_phase(Gamestate):-
    display_board(Gamestate),
    print_stats(Gamestate),
    repeat,
    put_piece_input(Gamestate, Put),
    true.
/*
    put_piece_move( Gamestate, Put, NewGamestate),
    !,
    true.

game_cycle_second_phase(Gamestate):-
    game_over(Gamestate, Winner),
    !,
    display_end(Gamestate, Winner).

game_cycle_second_phase(Gamestate):-
    display_board(Gamestate), 
    print_stats(Gamestate),
    choose_piece(Gamestate, Move),
    move_input(Gamestate, Move),
    move(Gamestate, Move, NewGamestate),
    game_cycle_second_phase(NewGamestate).

*/

put_piece_input(Gamestate, [Row, Column]) :-
    repeat,
    write('Choose a position to put a piece: '),
    read(Input),
    atom_chars(Input, [RowChar, ColumnChar|_]), % Convert input to a list of characters
    char_code(RowChar, RowCode),
    char_code(ColumnChar, ColumnCode),
    Row is RowCode, %  row value
    Column is ColumnCode - 48, %  column value
    valid_put_piece([Row, Column], 11),  % change the last argument depending of board size
    write('Valid position: '), write([Row, Column]), nl,
    !.


valid_put_piece([Row, Column], Size) :-
    valid_row(Row, Size),
    valid_column(Column, Row, Size).

valid_row(Row, Size) :-
    write(Row), nl,
    Row >= 97, %  'a'
    Row < 97 + Size. %  'a' + SizeX

valid_column(Column, Row, Size) :-
    integer(Column),
    Column >= 1,
    Column =< Size,
    valid_max_column(Row, Size, MaxColumn),
    Column =< MaxColumn,
    !.

valid_max_column(Row, Size, MaxColumn) :-
    SizeHalf is ceiling(Size / 2),
    (Row < 97 + SizeHalf ->
        MaxColumn is abs(Row - 97 + ceiling(Size/2))
    ;
        MaxColumn is abs(97 + Size - Row + SizeHalf - 1)
    ).

/*

106

*/