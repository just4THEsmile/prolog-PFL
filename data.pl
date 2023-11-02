% name_of_the_player(+Player, -Name)
% Find the Players name
:- dynamic name_of_the_player/2.

% bot_difficulty(+Bot,-Difficulty)
% Find the Bot difficulty
:- dynamic bot_difficulty/2.

% disks(+Player, -Disks)
% Finds the number of disks yet to be placed
:- dynamic disks/2.

% white(+Player)
% Finds which Player is White
:- dynamic white/1.

% black(+Player)
% Finds which Player is Black
:- dynamic black/1.

% board(+Size,+Matrix)
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
