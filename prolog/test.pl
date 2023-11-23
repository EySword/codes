ifright(X):-
    (
        X > 0 ->
        (
            (X =:= 1, !);
            (X >= 3, !)
        );
        (
            write('smail than 0'),
            fail
        )
    ),
    write('should not be showed').

boot(X) :-
    X > 0, X < 10 ->
    (write(1));
    (write(0)).

comp(X) :-
    X =:= (1,1).