plus(X,Y,Z):-Z is X+Y.
plus2(X,Y):- 3 is X+Y.

recrusion(N,F):-recrusion(N,1,F).
recrusion(N,T,F):- N>0, T1 is T*N, N1 is N-1, recrusion(N1,T1,F).
recrusion(0,F,F).

data(one).
data(two).
data(three).
data(four).

no_cut(X) :- 
    data(X), write(X), tab(3), write('here'),nl, fail.
no_cut(X) :- write('End').

do_cut(X) :- data(X), write(X), tab(3), !, write('here'), nl, fail.
do_cut('End').

no_cut_2(X,Y) :- 
    data(X), data(Y).
no_cut_2(_X,_Y) :- write('End').

do_cut_2(X,Y) :- 
    data(X), !, data(Y).
do_cut_2(_X,_Y) :- write('End').

command_loop:-
    repeat,
    write('Enter command (end to exit): '),
    read(X),
    write(X), nl,
    X = end.

loop:-
    write('H'),
    repeat,
    write('Enter command (end to exit): '),
    read(X),
    write(X), nl,
    q=end,
    write('here. '),
    end=end.
