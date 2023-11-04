:-consult(data).
:-consult(display).
:-consult(utils).
:- use_module(library(lists)).


% game_cycle_first_phase(+Gamestate,+NewGamestate4)
% Cycles through the first phase of the game, where the players put their pieces on the board until there are no more pieces available.
game_cycle_first_phase(Gamestate,NewGamestate4):-
    clear,
    display_game(Gamestate),
    (disks(WhitePieces, BlackPieces), WhitePieces = 0, BlackPieces = 0) ->
        write('No more pieces available for both players. Moving to the next phase.'), nl,
        NewGamestate4 = Gamestate
    ;
        
        print_stats(Gamestate),
        put_piece_input(Gamestate, Put, white),
        put_piece_move(Gamestate, Put, NewGamestate, white),
        display_game(NewGamestate),
        put_piece_input(NewGamestate, Put2, black),
        put_piece_move(NewGamestate, Put2, NewGamestate2, black),
        change_player(NewGamestate2, NewGamestate3),
        game_cycle_first_phase(NewGamestate3,NewGamestate4).

% put_piece_input(+[Board,Player], -[Row, Column], +Color)
% Asks the user or a bot for a position to put a piece and checks if its valid
put_piece_input([Board,Player], [Row, Column], Color) :-
    disks(WhitePieces, BlackPieces),
    (Color = white, WhitePieces > 0; Color = black, BlackPieces > 0),
    (white(Player) -> ColorPlayer = white; black(Player) -> ColorPlayer = black),
    format('\nYour PlayerColor is ~a!\n', [ColorPlayer]),
    repeat,
    format('Choose a position to put a ~a piece: ', [Color]),
    getCoords([RowCode,Column|_],Player,Board,Color),
    Row is RowCode, %  row value
    length(Board,BoardSize),
    valid_put_piece([Row, Column], BoardSize),  %  check if the position is valid
    is_empty(Board,Row,Column),
    (Color = white -> decrement_white_pieces; Color =  black -> decrement_black_pieces), % Decrement pieces count based on color
    char_code(RowChar, RowCode),
    sleep(1),
    format('\nValid position: ~a~d\n', [RowChar, Column]),
    !.

put_piece_input(_,_,_):-
    fail.

% getCoords(-[RowCode, ColumnCode|_], +Player,+Board,+Color)
% Asks the user or a bot for a position to put a piece
getCoords([RowCode, ColumnCode|_], Player, Board, Color):-
    (
        (Player = 1 -> name_of_the_player(player1, Name); Player = 2 -> name_of_the_player(player2, Name)),
        \+ member(Name, ['FitBot', 'FatBot', 'Bot']), write(' '),
        read(Input),
        atom_chars(Input, InputChars), 
        parseInput(InputChars, RowChar, ColumnCode), % Parse input into row and column components
        char_code(RowChar, RowCode)
    );
    (
        (Player = 1 -> name_of_the_player(player1, Name), bot_difficulty(player1, Difficulty); 
        Player = 2 -> name_of_the_player(player2, Name), bot_difficulty(player2, Difficulty)),
        (
            Difficulty = 1 -> getCoordsRandom([RowCode, ColumnCode|_], Board);
            Difficulty = 2 -> getCoordsHard([RowCode, ColumnCode], [Board, Player], Color)
        )
    ).

% getCoordsRandom(-[Row, Column],+Board)
% Gets a random position to put a piece
getCoordsRandom([Row, Column],Board):-
    valid_moves(Board, ListOfMoves),
    random_member([Row,Column],ListOfMoves).

% getCoordsHard(-[Row, Column], +[Board, Player], +Color)
% Gets the best position to put a piece
getCoordsHard([Row, Column], [Board, Player], Color):-
    (white(Player) -> ColorPlayer = white; black(Player) -> ColorPlayer = black),
    (Color = ColorPlayer -> 
        get_own_pieces(Board, OwnPieces, ColorPlayer),
        (OwnPieces = [] -> 
            length(Board, Size),
            CenterRow is 97 + Size // 2,
            CenterColumn is Size // 2 + 1,
            choose_random_move([CenterRow, CenterColumn], [Row, Column]),
            format('Best Move: ~w\n', [[Row, Column]])
        ;
            valid_moves(Board, EmptyPositions),
            evaluate_proximity(EmptyPositions, OwnPieces, [_, [Row, Column]]),
            format('Best Move: ~w\n', [[Row, Column]])
        )
    ;
        (ColorPlayer = white -> ColorAdversary = black; ColorAdversary = white),
        get_own_pieces(Board, OpponentPieces, ColorAdversary),
        (OpponentPieces = [] -> 
            length(Board, Size),
            CenterRow is 97 + Size // 2,
            CenterColumn is Size // 2 + 1,
            choose_random_move_other( Board, [Row, Column]),
            format('Best Move: ~w\n', [[Row, Column]])
        ;
            write('Own Pieces: '), write(OpponentPieces), nl,
            valid_moves(Board, EmptyPositions),
            findall([DistanceNeg, [Row, Column]], (
                member([Row, Column], EmptyPositions),
                calculate_distances([Row, Column], OpponentPieces, Distances),
                sum_list(Distances, Distance),
                DistanceNeg is -Distance % Negate distance for sorting
            ), Moves),
            sort(Moves, SortedMoves),
            nth1(_, SortedMoves, [_, [Row, Column]]),
            format('Best Move: ~w\n', [[Row, Column]])
        )
    ).


%choose_random_move_other(+Board, -[Row, Column])
% Chooses a random tile to place a piece
choose_random_move_other( Board, [Row, Column]):-
    length(Board, Size),
    ExtremeCorner is 97 + Size - 1,
    ExtremeCorner2 is ceiling(Size/2),
    Corner1 = [97, 1],
    Corner2 = [97, ExtremeCorner2],
    Corner3 = [ExtremeCorner, 1],
    Corner4 = [ExtremeCorner, ExtremeCorner2],
    Corners = [Corner1, Corner2, Corner3, Corner4],
    random_member([Row, Column], Corners).

%choose_random_move(+[CenterRow, CenterColumn], -[Row, Column])
% Chooses a random tile to place a piece around the center of the board
choose_random_move([CenterRow, CenterColumn], [Row, Column]):-
    SurroundingTiles = [[-1, 0], [1, 0], [0, -1], [0, 1], [-1, -1], [1, -1]],
    random_member([RRow,RColumn],SurroundingTiles),
    Row is CenterRow + RRow,
    Column is CenterColumn + RColumn.

% calculate_distances(+[EmptyRow, EmptyColumn], +OwnPieces, -Distances)
% Calculates the distance between a tile and all the pieces of a player
calculate_distances([EmptyRow, EmptyColumn], OwnPieces, Distances):-
    findall(Distance, (
        member([OwnRow, OwnColumn], OwnPieces),
        manhattan_distance([OwnRow, OwnColumn], [EmptyRow, EmptyColumn], Distance)
    ), Distances).


% evaluate_proximity(+EmptyPositions, +OwnPieces, -BestMove)
% Evaluates the best move to place a piece
evaluate_proximity(EmptyPositions, OwnPieces, BestMove):-
    findall([SumDistances, [Row, Column]], (
        nth1(_, EmptyPositions, [Row, Column]),
        calculate_distances([Row, Column], OwnPieces, Distances),
        sum_list(Distances, SumDistances)
    ), Moves),
    sort(Moves, SortedMoves),
    write('Sorted Moves: '), write(SortedMoves), nl,
    nth1(_, SortedMoves, BestMove).

% valid_moves(+Board, -ListOfMoves)
% Gets all the valid moves for a board
valid_moves(Board, ListOfMoves):-
    length(Board, Size),
    Upper is 97 + Size,
    findall([R, C], (
        between(1,Size,C),
        between(97, Upper, R),
        valid_row(R, Size),
        valid_column(C, R, Size),
        not_in_center([R, C], Size),
        is_empty(Board, R, C)
    ), ListOfMoves).

% get_own_pieces(+Board, -ListofPieces, +ColorPlayer)
% Gets all the pieces of a player
get_own_pieces(Board,ListofPieces,ColorPlayer):-
    findall([R,C], (
        nth1(RowIndex, Board, BoardRow),
        nth1(ColumnIndex, BoardRow, ColorPlayer),
        R is 97 + RowIndex,
        C is ColumnIndex
    ), ListofPieces).

% valid_put_piece(+[Row, Column], +Size)
% Checks if a position is valid
valid_put_piece([Row, Column], Size):-
    valid_row(Row, Size),
    valid_column(Column, Row, Size),
    not_in_center([Row,Column],Size).

% valid_row(+Row, +Size)
% Checks if a row is valid
valid_row(Row, Size) :-
    Row >= 97, % ASCII code for 'a'
    Row < 97 + Size,
    !. % Cut to prevent backtracking if the row is valid

valid_row(Row, _) :-
    %char_code(RowChar,Row),
    %format('Invalid row: ~w~n', [RowChar]),
    fail.

% valid_column(+Column, +Row, +Size)
% Checks if a column is valid
valid_column(Column, Row, Size) :-
    integer(Column),
    Column >= 1,
    Column =< Size,
    valid_max_column(Row, Size, MaxColumn),
    Column =< MaxColumn,
    !.

valid_column(Column, _, _) :-
    %format('Invalid column: ~w~n', [Column]),
    fail.

% valid_max_column(+Row, +Size, +MaxColumn)
% Checks if a column is valid and according to the hexagonal board design
valid_max_column(Row, Size, MaxColumn):-
    SizeHalf is ceiling(Size / 2),
    (Row < 97 + SizeHalf ->
        MaxColumn is abs(Row - 97 + ceiling(Size/2))
    ;
        MaxColumn is abs(97 + Size - Row + SizeHalf - 1)
    ).

% not_in_center(+[Row, Column], +Size)
% Checks if a position is not in the center of the board
not_in_center([Row, Column], Size):-
    CenterRow is 97 + Size // 2,
    CenterColumn is Size // 2 + 1,
    (Row \= CenterRow ; Column \= CenterColumn).

% is_empty(+Board, +Row, +Column)
% Checks if a position is empty
is_empty(Board, Row, Column):-
    RowIndex is Row - 97 + 1,
    ColumnIndex is Column,
    nth1(RowIndex, Board, BoardRow),
    nth1(ColumnIndex, BoardRow, empty).

% put_piece_move(+[Board, Player], +[Row, Column], -[NewBoard, Player], +Color)
% Places a piece in a position and updates the board
put_piece_move([Board, Player], [Row, Column], [NewBoard, Player], Color):-
    RowIndex is Row - 97 + 1,
    nth1(RowIndex, Board, BoardRow),
    update_column(Column, Color, BoardRow, NewBoardRow),
    update_row(RowIndex, NewBoardRow, Board, NewBoard).

% update_column(+Column, +Element, +List, -UpdatedList)
% Updates a column of a board
update_column(1, Element, [_|Rest], [Element|Rest]).
update_column(Column, Element, [H|T], [H|UpdatedT]):-
    Column > 1,
    Column1 is Column - 1,
    update_column(Column1, Element, T, UpdatedT).

% update_row(+RowIndex, +NewRow, +List, -UpdatedList)
% Updates a row of a board
update_row(1, NewRow, [_|T], [NewRow|T]).
update_row(RowIndex, NewRow, [H|T], [H|UpdatedT]):-
    RowIndex > 1,
    RowIndex1 is RowIndex - 1,
    update_row(RowIndex1, NewRow, T, UpdatedT).

% decrement_white_pieces
% Decrements the number of white pieces
decrement_white_pieces:-
    retract(disks(WhitePieces, BlackPieces)),
    NewWhitePieces is WhitePieces - 1,
    asserta(disks(NewWhitePieces, BlackPieces)).

% decrement_black_pieces
% Decrements the number of black pieces
decrement_black_pieces:-
    retract(disks(WhitePieces, BlackPieces)),
    NewBlackPieces is BlackPieces - 1,
    asserta(disks(WhitePieces, NewBlackPieces)).

% change_player(+[Board, Player], -[Board, NewPlayer])
% Changes the current player
change_player([Board, Player], [Board, NewPlayer]):-
    (Player = 1 -> NewPlayer = 2; Player = 2 -> NewPlayer = 1).


game_cycle_second_phase(Gamestate):-
    display_game(Gamestate), 
    check_game_over(Gamestate,Winner),
    winner_screen(Gamestate,Winner).

game_cycle_second_phase(Gamestate):-
    print_stats(Gamestate),
    move_piece(Gamestate, NewGamestate),
    change_player(NewGamestate, NewGamestate2),
    game_cycle_second_phase(NewGamestate2).





%------------------------------------functions to check if victory conditions are met ----------------------------------------------------------------------------

check_game_over(Gamestate,Winner):-
    check_if_white_wins(Gamestate,Winner).

check_game_over(Gamestate,Winner):-
    check_if_black_wins(Gamestate,Winner).    

check_game_over(_):-
    !,
    fail.

check_if_black_wins([Matrix,Player],Winner):-
    %write('black wins:'),nl,
    length(Matrix,Num_rows),
    Side_size is ((Num_rows+1)//2),
    find_Values(blackblack, blackwhite, Matrix,Indices),
    %write(Indices),nl,
    check_3in_line(Indices, black, Side_size),
    Winner is Player,
    write(Indices),nl.





check_if_white_wins([Matrix,Player],Winner):-
    %write('white wins:'),nl,
    length(Matrix,Num_rows),
    Side_size is ((Num_rows+1)//2),
    find_Values(whiteblack, whitewhite, Matrix,Indices),
    %write(Indices),nl,
    check_3in_line(Indices, white, Side_size),
    Winner is Player,
    write(Indices),nl.

% auxilliary fuctions ------------------------------------

% Define a predicate to find the indices of two values in a matrix and return them as a list
find_Values( Value1, Value2,Matrix, Indices) :-
    find_indices(Matrix, 1, Value1, Value2, [], Indices),!.

% Base case: When all rows are checked
find_indices([], _, _, _, Indices, Indices).

% Iterate through each row of the matrix
find_indices([Row|Rest], RowIndex, Value1, Value2, Acc, Indices) :-
    find_indices_in_row(Row, RowIndex, 1, Value1, Value2, [], RowIndices),
    NextRowIndex is RowIndex + 1,
    append(Acc, RowIndices, NewAcc),
    find_indices(Rest, NextRowIndex, Value1, Value2, NewAcc, Indices).

% Base case: When all columns in a row are checked
find_indices_in_row([], _, _, _, _, Indices, Indices).

% Iterate through each column in a row
find_indices_in_row([Element|Rest], RowIndex, ColIndex, Value1, Value2, Acc, Indices) :-
    Element = Value2,
    append(Acc, [RowIndex-ColIndex], NewAcc),
    NextColIndex is ColIndex + 1,
    find_indices_in_row(Rest, RowIndex, NextColIndex, Value1, Value2, NewAcc, Indices).

find_indices_in_row([Element|Rest], RowIndex, ColIndex, Value1, Value2, Acc, Indices) :-
    Element = Value1,
    % If the current element matches the first desired value, add its indices to the list
    append(Acc, [(RowIndex-ColIndex)], NewAcc),
    NextColIndex is ColIndex + 1,
    find_indices_in_row(Rest, RowIndex, NextColIndex, Value1, Value2, NewAcc, Indices).

find_indices_in_row([_|Rest], RowIndex, ColIndex, Value1, Value2, Acc, Indices) :-
        % If neither of the desired values is found, just continue to the next column
        NextColIndex is ColIndex + 1,
        find_indices_in_row(Rest, RowIndex, NextColIndex, Value1, Value2, Acc, Indices).



% end of auxilliary fuctions ----------------------------------------------  

check_3in_line([],_,_):- 
!,
fail.

check_3in_line([Row-Col|Tail],_,Side_size):-
    one_in_line_aux(Row-Col, Tail, Side_size),!.

check_3in_line([_|Tail],Player,Side_size):-
    check_3in_line(Tail, Player, Side_size).


%for horizontal lines
one_in_line_aux(Row-Col,List,_):-

    Col2 is Col+1,
    member(Row-Col2,List),

    Col3 is Col2+1,
    member(Row-Col3,List).

%for horizontal lines
one_in_line_aux(Row-Col,List,_):-
    Col2 is Col-1,
    member(Row-Col2,List),
    Col3 is Col2-1,
    member(Row-Col3,List).  

%for diagonal lines

%from top right to bottom left
one_in_line_aux(Row-Col,List,Side_size):-
    Row < Side_size,
    Row2 is Row+1,
    member(Row2-Col,List),
    Row2 < Side_size,
    Row3 is Row2+1,
    member(Row3-Col,List).

one_in_line_aux(Row-Col,List,Side_size):-
    Row < Side_size,
    Row2 is Row+1,
    member(Row2-Col,List),
    Row2 >= Side_size,
    Row3 is Row2+1,
    Col2 is Col-1,
    member(Row3-Col2,List).


one_in_line_aux(Row-Col,List,Side_size):-
    Row >= Side_size,
    Row2 is Row+1,
    Col2 is Col-1,
    member(Row2-Col2,List),
    Row3 is Row2+1,
    Col3 is Col2-1,
    member(Row3-Col3,List).

%from top left to bottom right

one_in_line_aux(Row-Col,List,Side_size):-
    Row < Side_size,
    Row2 is Row+1,
    Col2 is Col+1,
    member(Row2-Col2,List),
    Row2 < Side_size,
    Row3 is Row2+1,
    Col3 is Col2+1,
    member(Row3-Col3,List).

one_in_line_aux(Row-Col,List,Side_size):-
    Row < Side_size,
    Row2 is Row+1,
    Col2 is Col+1,
    member(Row2-Col2,List),
    Row2 >= Side_size,
    Row3 is Row2+1,
    member(Row3-Col2,List).


one_in_line_aux(Row-Col,List,Side_size):-
    Row >= Side_size,
    Row2 is Row+1,
    member(Row2-Col,List),
    Row3 is Row2+1,
    member(Row3-Col,List).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
inBoard(Row-Col,Board):-
    length(Board,Size),
    valid_row(Row, Size),
    valid_column(Col, Row, Size).

check_if_piece_is_from_player(Row-Col,Player,Board,Range):-
    white(Player),
    find_Values(whiteblack, whitewhite, Board, Indices),
    member(Row-Col,Indices),
    Range is 2.

check_if_piece_is_from_player(Row-Col,Player,Board,Range):-
    white(Player),
    find_Values(white, white, Board, Indices),
    member(Row-Col,Indices),
    Range is 1.

check_if_piece_is_from_player(Row-Col,Player,Board,Range):-
    black(Player),
    find_Values(blackblack, blackwhite, Board, Indices),
    member(Row-Col,Indices),
    Range is 2.

check_if_piece_is_from_player(Row-Col,Player,Board,Range):-
    black(Player),
    find_Values(black, black, Board, Indices),
    member(Row-Col,Indices),
    Range is 1.   

check_if_destination_is_clear_or_has_one_level(Row-Col,Board):-
    find_Values(whiteblack, whitewhite, Board, Indices),
    \+ member(Row-Col,Indices),
    find_Values(blackblack, blackwhite, Board, Indices2),
    \+ member(Row-Col,Indices2).


move_piece([Board,Player],[NewBoard,Player]):-
    (Player = 1 ->
        name_of_the_player(player1, Name)
    ; Player = 2 ->
        name_of_the_player(player2, Name)
    ),
    \+ member(Name, ['FitBot', 'FatBot', 'Bot']),
    format('Player: ~a choose the piece you want to move', [Name]),nl,
    getCoords([Row, Col], Player, Board, _),
    RealRow is Row-97+1,
    check_if_piece_is_from_player(RealRow-Col,Player,Board,Range),
    format('Player: ~a choose a position to move the piece to:', [Name]), nl,
    getCoords([Row2, Col2], Player, Board,_),
    inBoard(Row2-Col2,Board),
    RealRow2 is Row2-97+1,
    length(Board,Size),
    Side_Size is ((Size+1)//2),
    get_distance_in_line(RealRow-Col,RealRow2-Col2,Side_Size,CheckRange),
    CheckRange =:= Range,nl,
    check_if_destination_is_clear_or_has_one_level(RealRow2-Col2,Board),
    move_aux(RealRow-Col,RealRow2-Col2,Board,NewBoard, Player).

move_piece([Board,Player],[NewBoard,Player]):-
    (Player = 1 ->
        name_of_the_player(player1, Name)
    ; Player = 2 ->
        name_of_the_player(player2, Name)
    ),
    member(Name, ['FitBot', 'FatBot', 'Bot']),write(Name),nl,
    (Player = 1 ->
        bot_difficulty(player1, Difficulty)
    ; Player = 2 ->
        bot_difficulty(player2, Difficulty)
    ),
    (
        Difficulty = 1 -> get_random_move_second_cycle([R-C,R1-C1], [Board, Player]);
        Difficulty = 2 -> get_Hard_Move_second_cycle([R-C,R1-C1], [Board, Player])
    ),
    write('Bot chose: '), write([R-C,R1-C1]), nl,
    move_aux(R-C,R1-C1,Board,NewBoard, Player).


  
move_piece([Board,Player],[NewBoard,Player]):-
    write('Move not valid. Try again.'), nl,
    move_piece([Board,Player],[NewBoard,Player]).     

  


move_aux(Row-Col,Row2-Col2,Board,NewNewBoard, Player):-
    white(Player),
    remove_top_piece(Row-Col, Board,NewBoard, Player),
    add_top_piece(Row2-Col2, white, NewBoard , NewNewBoard,Player).    

move_aux(Row-Col,Row2-Col2,Board,NewNewBoard, Player):-
    black(Player),
    remove_top_piece(Row-Col, Board,NewBoard, Player),
    add_top_piece(Row2-Col2, black, NewBoard , NewNewBoard,Player).      
    
remove_top_piece(Row-Col ,Board, NewBoard, Player):-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, PieceColor),

        (((PieceColor = white) ; (PieceColor = black) )->

            NewColor = empty;
        ((PieceColor = whitewhite) ; (PieceColor = blackwhite) ) ->

            NewColor = white;

        ((PieceColor = whiteblack) ; (PieceColor = blackblack) ) ->

            NewColor = black
        ),
    AddaptedRow is Row + 97 - 1,
    put_piece_move([Board, Player], [AddaptedRow, Col], [NewBoard, Player], NewColor).  


add_top_piece(Row-Col, Color, Board,NewBoard, Player):-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, PieceColor),
    (PieceColor = white->

        (Color = white ->

            NewColor = whitewhite;

        Color = black ->

            NewColor = blackwhite);
    PieceColor = black  ->

        (Color = white ->

            NewColor = whiteblack;

        Color = black ->

            NewColor = blackblack);
        
        
    PieceColor = empty  ->

        NewColor = Color
    ),
    AddaptedRow is Row + 97 - 1,
    put_piece_move([Board, Player], [AddaptedRow, Col], [NewBoard, Player], NewColor).     

%    get_distance_in_line

%-------------range-1-----------------------------------------------------------------------------------------------------------------------------------------------
get_distance_in_line(Row-Col,Row2-Col2,_,Range):-
    Row =:= Row2,
    Range is abs(Col-Col2).

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row > Side_size,Row =:= (Row2 + 1), 
    (Col=:=Col2; Col=:=Col2-1),
    Range is 1.

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row > Side_size,Row =:= (Row2 - 1), 
    (Col=:=Col2; Col=:=Col2+1),
    Range is 1.    

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row < Side_size,Row =:= (Row2 + 1), 
    (Col=:=Col2; Col=:=Col2+1),
    Range is 1.

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row < Side_size,Row =:= (Row2 - 1), 
    (Col=:=Col2; Col=:=Col2-1),
    Range is 1.        

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row =:= Side_size,(Row =:= (Row2 - 1) ; Row =:= (Row2 + 1)), 
    (Col=:=Col2; Col=:=Col2+1),
    Range is 1.    
%----range-2------------------------------------------------------------------------------------------------------------------------------------------------------
get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row =:= Side_size, (Row =:= (Row2 - 2) ; Row =:= (Row2 + 2)), 
    (Col=:=Col2; Col=:=Col2-2),
    Range is 2.

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row =:= Side_size-1, Row =:= (Row2 - 2), 
    (Col=:=Col2-1; Col=:=Col2+1),
    Range is 2.    

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row =:= Side_size+1, Row =:= (Row2 + 2), 
    (Col=:=Col2-1; Col=:=Col2+1),
    Range is 2.        

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row > Side_size-1, Row =:= (Row2 - 2), 
    (Col=:=Col2; Col=:=Col2+2),
    Range is 2.

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row > Side_size-1, Row =:= (Row2 + 2), 
    (Col=:=Col2 -2; Col=:=Col2),
    Range is 2.    

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row < Side_size+1, Row =:= (Row2 - 2), 
    (Col=:=Col2 - 2; Col=:=Col2),
    Range is 2.

get_distance_in_line(Row-Col,Row2-Col2,Side_size,Range):-
    Row < Side_size+1, Row =:= (Row2 + 2), 
    (Col=:=Col2 ; Col=:=Col2 + 2),
    Range is 2.        
    

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------
%random  bot
inBoard_int(Row-Col,Board):-
    length(Board,Size),
    valid_row_int(Row, Size),
    valid_column_int(Col, Row, Size).    

valid_row_int(Row, Size) :-
    Row > 0, %  'a'
    Row =< Size. 

% valid_column(Column, Row, Size)
% Checks if a column is valid
valid_column_int(Column, Row, Size) :-
    integer(Column),
    Column >= 1,
    Column =< Size,
    valid_max_column_int(Row, Size, MaxColumn),
    Column =< MaxColumn,
    !.

% valid_max_column(Row, Size, MaxColumn)
% Checks if a column is valid and according to the hexagonal board design
valid_max_column_int(Row, Size, MaxColumn):-
    SizeHalf is ((Size // 2)+1),
    (Row < SizeHalf ->
        MaxColumn is abs(Row + SizeHalf - 1)
    ;
        MaxColumn is abs( Size - Row + SizeHalf )
    ).    






get_random_move_second_cycle([R-C,R1-C1], [Board, Player]):-
    valid_moves_second_cycle(Board,Player, ListOfMoves),
    random_member([R-C,R1-C1],ListOfMoves).


        

valid_moves_second_cycle(Board, Player,ListOfMoves):-
    white(Player),
    length(Board, Size),
    Side_Size is ((Size+1)//2),  
    findall([R-C,R1-C1], (
        between(1,Size,C),
        between(1, Size, R),
        inBoard_int(R-C,Board),
        between(1,Size,C1),
        between(1,Size,R1),
        inBoard_int(R1-C1,Board),
        find_Values(whiteblack, whitewhite, Board, Indices),
        find_Values(white, white, Board, Indices2),
        append(Indices,Indices2,Indices3),
        member(R-C,Indices3),
        check_if_piece_is_from_player(R-C,Player,Board,Range),
        get_distance_in_line(R-C,R1-C1,Side_Size,Range1),
        Range1 =:= Range,
        check_if_destination_is_clear_or_has_one_level(R1-C1,Board)
    ), ListOfMoves).   

valid_moves_second_cycle(Board, Player,ListOfMoves):-
    black(Player),
    length(Board, Size),
    Side_Size is ((Size+1)//2),  
    findall([R-C,R1-C1], (
        between(1,Size,C),
        between(1, Size, R),
        inBoard_int(R-C,Board),
        between(1,Size,C1),
        between(1,Size,R1),
        inBoard_int(R1-C1,Board),
        find_Values(blackblack, blackwhite, Board, Indices),
        find_Values(black, black, Board, Indices2),
        append(Indices,Indices2,Indices3),
        member(R-C,Indices3),
        check_if_piece_is_from_player(R-C,Player,Board,Range),
        get_distance_in_line(R-C,R1-C1,Side_Size,Range1),
        Range1 =:= Range,
        check_if_destination_is_clear_or_has_one_level(R1-C1,Board)
    ), ListOfMoves).     

calculate_distances_Score(R-C,R1-C1, OwnPieces, Scores,Player,Board):-
findall([Score,[OwnRow-OwnColumn]], (
    member(OwnRow-OwnColumn, OwnPieces),
    hexagonal_distance(OwnRow-OwnColumn, R1-C1, Distance),
    get_Move_score(R-C,R1-C1,Distance,Board,Score,Player)

), Scores).


% Define a predicate to calculate the hexagonal distance between two positions
hexagonal_distance(X1-Y1, X2-Y2, Distance) :-
    % Calculate the absolute differences in X and Y coordinates
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    % Calculate the horizontal distance, taking into account the "staggered" rows
    HDistance is DX,
    % Calculate the vertical distance, taking into account the "staggered" rows
    VDistance is DY + (DX // 2),
    % The maximum distance is the maximum of horizontal and vertical distances
    Distance is max(HDistance, VDistance).

get_Hard_Move_second_cycle(Move,[Board,Player]):-
    hard_moves_second_part(Board,Player,ListOfMoves),
    write(ListOfMoves),nl,
    sort(ListOfMoves,ListOfMoves2),
    nth1(1,ListOfMoves2,[_,Move]),
    write(Move),nl,nl,nl,nl,nl.


hard_moves_second_part(Board, Player,ListOfMoves):-
    white(Player),
    length(Board, Size),
    Side_Size is ((Size+1)//2),  
    findall([Score,[R-C,R1-C1]], (
        between(1,Size,C),
        between(1, Size, R),
        inBoard_int(R-C,Board),
        between(1,Size,C1),
        between(1,Size,R1),
        inBoard_int(R1-C1,Board),
        find_Values(whiteblack, whitewhite, Board, Indices),
        find_Values(white, white, Board, Indices2),
        append(Indices,Indices2,Indices3),
        member(R-C,Indices3),
        check_if_piece_is_from_player(R-C,Player,Board,Range),
        get_distance_in_line(R-C,R1-C1,Side_Size,Range1),
        Range1 =:= Range,
        check_if_destination_is_clear_or_has_one_level(R1-C1,Board),
        calculate_distances_Score(R-C,R1-C1, Indices3, Scores,Player,Board),
        sort(Scores,Scores2),
        nth1(1,Scores2,[Score,[_-_]])


    ), ListOfMoves).  

hard_moves_second_part(Board, Player,ListOfMoves):-
    black(Player),
    length(Board, Size),
    Side_Size is ((Size+1)//2),  
    findall([Score,[R-C,R1-C1]], (
        between(1,Size,C),
        between(1, Size, R),
        inBoard_int(R-C,Board),
        between(1,Size,C1),
        between(1,Size,R1),
        inBoard_int(R1-C1,Board),
        find_Values(blackblack, blackwhite, Board, Indices),
        find_Values(black, black, Board, Indices2),
        append(Indices,Indices2,Indices3),
        member(R-C,Indices3),
        check_if_piece_is_from_player(R-C,Player,Board,Range),
        get_distance_in_line(R-C,R1-C1,Side_Size,Range1),
        Range1 =:= Range,
        check_if_destination_is_clear_or_has_one_level(R1-C1,Board),
        calculate_distances_Score(R-C,R1-C1, Indices3, Scores,Player,Board),
        sort(Scores,Scores2),
        nth1(1,Scores2,[Score,[_-_]])


    ), ListOfMoves).     



get_Move_score(R-C,R1-C1,_,Board,Score,Player):-
    move_aux(R-C,R1-C1,Board,NewBoard, Player),
    check_game_over([NewBoard,Player],_),
    write([R-C,R1-C1]),nl,
    Score is -999,!.

get_Move_score(R-C,R1-C1,Distance,Board,Score,Player):-
    check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num),
    \+check_if_destination_is_clear_or_has_one_level(R-C,Board),
    Score is Distance+4 - Num*2,!.



get_Move_score(R-C,R1-C1,Distance,Board,Score,Player):-
    check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num),
    check_if_piece_is_from_player(R1-C1,Player,Board,_),
    Score is Distance -1 - Num*2,!.



get_Move_score(R-C,R1-C1,Distance,Board,Score,Player):-
    check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num),
    change_player([_,Player],[_,NextPlayer]),
    check_if_piece_is_from_player(R1-C1,NextPlayer,Board,_),
    Score is Distance - 2 - Num*2,!.



get_Move_score(R-C,R1-C1,Distance,Board,Score,Player):-
    check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num),
    Score is Distance - Num*2,!.      




check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num):-
    white(Player),
    length(Board, Size),
    Side_Size is ((Size+1)//2),    
    find_Values(whiteblack, whitewhite, Board, Indices),
    findall(R2-C2,(
        member(R2-C2,Indices),
        get_distance_in_line(R-C,R2-C2,Side_Size,Range),
        Range =:= 1
    ),List1),
    length(List1,NUM1),
    findall(R3-C3,(
        member(R2-C2,Indices),
        member(R3-C3,Indices),
        \+((R1=:=R3 , C1=:=C3)),
        get_distance_in_line(R-C,R2-C2,Side_Size,Range),
        Range =:= 1,
        get_distance_in_line(R-C,R3-C3,Side_Size,Range1),
        Range1 =:= 2,
        get_distance_in_line(R2-C2,R3-C3,Side_Size,Range2),
        Range2 =:= 1
    ),List3),
    length(List3,NUM3),

    findall(R3-C3,(
        member(R2-C2,Indices),
        member(R3-C3,Indices),
        \+((R=:=R3 , C=:=C3)),      
        get_distance_in_line(R1-C1,R2-C2,Side_Size,Range),
        Range =:= 1,
        get_distance_in_line(R1-C1,R3-C3,Side_Size,Range1),
        Range1 =:= 2,
        get_distance_in_line(R2-C2,R3-C3,Side_Size,Range2),
        Range2 =:= 1
    ),List4),
    length(List4,NUM4),

    findall(R2-C2,(
        member(R2-C2,Indices),
        get_distance_in_line(R1-C1,R2-C2,Side_Size,Range),
        Range =:= 1
    ),List2),
    length(List2,NUM2),
    Num is NUM2 - NUM1 - NUM3*10 + NUM4*10.     


check_delta_of_how_many_stacked_pieces_nearby(R-C,R1-C1,Board,Player,Num):-
    black(Player), 
    length(Board, Size),
    Side_Size is ((Size+1)//2),
    find_Values(blackblack, blackwhite, Board, Indices),
    findall(R2-C2,(
        member(R2-C2,Indices),
        get_distance_in_line(R-C,R2-C2,Side_Size,Range),
        Range =:= 1
    ),List1),
    length(List1,NUM1),
    findall(R3-C3,(
        member(R2-C2,Indices),
        member(R3-C3,Indices),
        \+((R1=:=R3 , C1=:=C3)),
        get_distance_in_line(R-C,R2-C2,Side_Size,Range),
        Range =:= 1,
        get_distance_in_line(R-C,R3-C3,Side_Size,Range1),
        Range1 =:= 2,
        get_distance_in_line(R2-C2,R3-C3,Side_Size,Range2),
        Range2 =:= 1
    ),List3),
    length(List3,NUM3),

    findall(R3-C3,(
        member(R2-C2,Indices),
        member(R3-C3,Indices),
        \+((R=:=R3 , C=:=C3)),        
        get_distance_in_line(R1-C1,R2-C2,Side_Size,Range),
        Range =:= 1,
        get_distance_in_line(R1-C1,R3-C3,Side_Size,Range1),
        Range1 =:= 2,
        get_distance_in_line(R2-C2,R3-C3,Side_Size,Range2),
        Range2 =:= 1
    ),List4),
    length(List4,NUM4),

    findall(R2-C2,(
        member(R2-C2,Indices),
        get_distance_in_line(R1-C1,R2-C2,Side_Size,Range),
        Range =:= 1
    ),List2),
    length(List2,NUM2),
    Num is NUM2 - NUM1 - NUM3*10 + NUM4*10.   
    
