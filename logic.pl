:-consult(data).
:-consult(display).

/*game_cycle_first_phase(Gamestate):-
    first_phase_over(Gamestate).
*/
game_cycle_first_phase(Gamestate):-
    (disks(WhitePieces, BlackPieces), WhitePieces = 0, BlackPieces = 0) ->
        write('No more pieces available for both players. Moving to the next phase.'), nl
    ;
        display_board(Gamestate),
        print_stats(Gamestate),
        put_piece_input(Gamestate, Put, white),
        put_piece_move(Gamestate, Put, NewGamestate, white),
        display_board(NewGamestate),
        put_piece_input(NewGamestate, Put2, black),
        put_piece_move(NewGamestate, Put2, NewGamestate2, black),
        change_player(NewGamestate2, NewGamestate3),
        game_cycle_first_phase(NewGamestate3).


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


put_piece_input([Board,Player], [Row, Column], Color) :-
    disks(WhitePieces, BlackPieces),
    (Color = white, WhitePieces > 0; Color = black, BlackPieces > 0),
    repeat,
    format('Choose a position to put a ~a piece: ', [Color]),
    read(Input),
    atom_chars(Input, [RowChar, ColumnChar|_]), % Convert input to a list of characters
    char_code(RowChar, RowCode),
    char_code(ColumnChar, ColumnCode),
    Row is RowCode, %  row value
    Column is ColumnCode - 48, %  column value
    valid_put_piece([Row, Column], 9),  % change the last argument depending of board size
    is_empty(Board,Row,Column),
    (Color = white -> decrement_white_pieces; Color =  black -> decrement_black_pieces), % Decrement pieces count based on color
    format('Valid position: ~a~d\n', [RowChar, Column]),
    !.

put_piece_input(_,_,Color):-
    fail.

valid_put_piece([Row, Column], Size) :-
    valid_row(Row, Size),
    valid_column(Column, Row, Size).

valid_row(Row, Size) :-
    Row >= 97, %  'a'
    Row < 97 + Size. 

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

is_empty(Board, Row, Column) :-
    RowIndex is Row - 97 + 1,
    ColumnIndex is Column,
    nth1(RowIndex, Board, BoardRow),
    nth1(ColumnIndex, BoardRow, empty).


put_piece_move([Board, Player], [Row, Column], [NewBoard, Player], Color) :-
    RowIndex is Row - 97 + 1,
    nth1(RowIndex, Board, BoardRow),
    update_column(Column, Color, BoardRow, NewBoardRow),
    update_row(RowIndex, NewBoardRow, Board, NewBoard).

update_column(1, Element, [_|Rest], [Element|Rest]).
update_column(Column, Element, [H|T], [H|UpdatedT]) :-
    Column > 1,
    Column1 is Column - 1,
    update_column(Column1, Element, T, UpdatedT).

update_row(1, NewRow, [_|T], [NewRow|T]).
update_row(RowIndex, NewRow, [H|T], [H|UpdatedT]) :-
    RowIndex > 1,
    RowIndex1 is RowIndex - 1,
    update_row(RowIndex1, NewRow, T, UpdatedT).

decrement_white_pieces:-
    retract(disks(WhitePieces, BlackPieces)),
    NewWhitePieces is WhitePieces - 1,
    asserta(disks(NewWhitePieces, BlackPieces)).

decrement_black_pieces:-
    retract(disks(WhitePieces, BlackPieces)),
    NewBlackPieces is BlackPieces - 1,
    asserta(disks(WhitePieces, NewBlackPieces)).

change_player([Board, Player], [Board, NewPlayer]) :-
    (Player = 1 -> NewPlayer = 2; Player = 2 -> NewPlayer = 1).