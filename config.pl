drawMenuPlayer:-
    clear,
    write('                           Hello and Welcome to DuAlma Game.\n\n\n'),
    write(' Select your desired gamemode:\n'),
    write('     1. Human vs Human\n'),
    write('     2. Human vs Bot\n'),
    write('     3. Bot vs Bot\n'),
    write(' Please Insert a Number, with a dot in the end, to select the gamemode.\n'),
    handleInputMenuPlayer.

handleInputMenuPlayer:-
    read(MenuInput),
    (between(1, 3, MenuInput) ->
        (assert(gamemode(MenuInput))),
        (MenuInput =:= 1 -> true ; drawMenuDifficulty)
    ;
        write('Wrong Number. Please insert again.\n'),
        handleInputMenuPlayer
    ).


drawMenuDifficulty:-
    clear,
    write(' Select your desired difficulty:\n'),
    write('     1. Level Easy\n'),
    write('     2. Level Hard\n'),
    write(' Please Insert a Number, with a dot in the end, to play DuAlma.\n'),
    handleInputMenuDifficulty.

handleInputMenuDifficulty:-
    read(MenuInput),
    (between(1, 2, MenuInput) ->
        (assert(difficulty(MenuInput))),
        true
    ;
        write('Wrong Number. Please insert again.\n'),
        handleInputMenuDifficulty
    ).

