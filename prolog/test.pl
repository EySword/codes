parse():-parse(X,Y).
parse(X,Y):-read(X).
parse(X,[Y|Z]):-delta(Y,Z), write(X).

delta(a,b).