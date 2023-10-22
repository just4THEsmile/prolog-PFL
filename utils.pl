clear:- 
    write('\33\[2J').

    get_name(Player):-
        format('Hello ~a, what is your name? ', [Player]),
        read(Name),
        asserta(name_of(Player, Name)).