:- dynamic name_of_the_player/2.
:- dynamic difficulty/2.
:- dynamic player_pieces/3. % player_pieces(Player, whitePiecesNumber, blackPiecesNumber)


board(5,[
         [empty, empty, empty, empty, empty],
      [empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
   [empty, empty, empty, empty, empty, empty, empty, empty],
[empty, empty, empty, empty, empty, empty, empty, empty, empty],
   [empty, empty, empty, empty, empty, empty, empty, empty],     
     [empty, empty, empty, empty, empty, empty, empty],
      [empty, empty, empty, empty, empty, empty],
         [empty, empty, empty, empty, empty]
]).

board(6,[
         [empty, empty, empty, empty, empty, empty],
         [empty, empty, empty, empty, empty, empty ,empty],
      [empty, empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty, empty, empty],
   [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
[empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
   [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],     
     [empty, empty, empty, empty, empty, empty, empty, empty, empty],
      [empty, empty, empty, empty, empty, empty, empty, empty],
         [empty, empty, empty, empty, empty, empty, empty],
             [empty, empty, empty, empty, empty, empty]
]).
