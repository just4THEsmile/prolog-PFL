:- dynamic name_of_the_player/2.
:- dynamic difficulty/2.
:- dynamic disks/2. % disks(numWhites,numBlacks)


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
