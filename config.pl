drawMenuPlayer:-
    write('                           Hello and Welcome to DuAlma Game.\n\n\n'),
    write(' Select your desired gamemode:\n'),
    write('     1. Human vs Human\n'),
    write('     2. Human vs Bot\n'),
    write('     3. Bot vs Bot\n'),
    write(' Please Insert a Number, with a dot in the end, to select the gamemode.\n'),
    handleInput(3).

drawMenuDifficulty:-
    write(' Select your desired difficulty:\n'),
    write('     1. Level Easy\n'),
    write('     2. Level Hard\n'),
    write(' Please Insert a Number, with a dot in the end, to play DuAlma.\n'),
    handleInput(2).

handleInput(ValidInputs):-
    read(MenuInput),
    (between(1, ValidInputs, MenuInput) ->
        !
    ;
        write('Wrong Number. Please insert again.\n'),
        handleInput(ValidInputs)
    ).
