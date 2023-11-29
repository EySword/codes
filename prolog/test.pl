square(X, Y) :- Y is X * X.
delayed_square(X, Y) :- when(nonvar(X), square(X, Y)).

tes(X,Y) :- square(X,Y).