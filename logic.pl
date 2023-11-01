:-consult(data).
:-consult(display).
:- use_module(library(system)).

/*game_cycle_first_phase(Gamestate):-
    first_phase_over(Gamestate).
*/
game_cycle_first_phase(Gamestate):-
    display_board(Gamestate),
    (disks(WhitePieces, BlackPieces), WhitePieces = 0, BlackPieces = 0) ->
        write('No more pieces available for both players. Moving to the next phase.'), nl
    ;
        
        print_stats(Gamestate),
        put_piece_input(Gamestate, Put, white),
        put_piece_move(Gamestate, Put, NewGamestate, white),
        display_board(NewGamestate),
        put_piece_input(NewGamestate, Put2, black),
        put_piece_move(NewGamestate, Put2, NewGamestate2, black),
        change_player(NewGamestate2, NewGamestate3),
        game_cycle_first_phase(NewGamestate3).


put_piece_input([Board,Player], [Row, Column], Color) :-
    disks(WhitePieces, BlackPieces),
    (Color = white, WhitePieces > 0; Color = black, BlackPieces > 0),
    repeat,
    format('Choose a position to put a ~a piece: ', [Color]),
    getCoords([RowCode,Column|_],Player,Board),
    Row is RowCode, %  row value
    length(Board,BoardSize),
    valid_put_piece([Row, Column], BoardSize),  %  check if the position is valid
    is_empty(Board,Row,Column),
    (Color = white -> decrement_white_pieces; Color =  black -> decrement_black_pieces), % Decrement pieces count based on color
    char_code(RowChar, RowCode),
    format('Valid position: ~a~d\n', [RowChar, Column]),
    !.

put_piece_input(_,_,Color):-
    fail.

getCoords([RowCode, ColumnCode|_], Player,Board):-
    (
        (Player = 1 -> name_of_the_player(player1, Name); Player = 2 -> name_of_the_player(player2, Name)),
        \+ member(Name, ['FitBot', 'FatBot', 'Bot']),
        read(Input),
        atom_chars(Input, [RowChar, ColumnChar|_]), % Convert input to a list of characters
        char_code(RowChar, RowCode),
        char_code(ColumnChar, ColumnCodeUnrefined),
        ColumnCode is ColumnCodeUnrefined - 48 %  column value   
    );
    (
        (Player = 1 -> name_of_the_player(player1, Name), bot_difficulty(player1, Difficulty); 
        Player = 2 -> name_of_the_player(player2, Name), bot_difficulty(player2, Difficulty)),
        (
            Difficulty = 1 -> getCoordsRandom([RowCode, ColumnCode|_],Board);
            Difficulty = 2 -> getCoordsHard([RowCode, ColumnCode|_],Board)
        )
    ).

getCoordsRandom([Row, Column],Board):-
    length(Board,Size),
    MaxRow is 97 + Size,
    random(97, MaxRow, Row),
    random(1, 10, Column).

valid_put_piece([Row, Column], Size) :-
    valid_row(Row, Size),
    valid_column(Column, Row, Size),
    not_in_center([Row,Column],Size).

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

not_in_center([Row,Column],Size):-
    Center is Size // 2 + 1,
    Row \= 97 + Center,
    Column \= Center.

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
