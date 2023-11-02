:- dynamic name_of_the_player/2.
:- dynamic bot_difficulty/2.
:- dynamic disks/2. % disks(numWhites,numBlacks)
:- dynamic white/1.
:- dynamic black/1.


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
