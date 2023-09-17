select(X,[X|Xs],Xs) :- write('1').
select(X,[Y|Ys],[Y|Zs]) :- write('2'), select(X,Ys,Zs).

maximum(X,Y,X) :- X >= Y.
maximum(X,Y,Y) :- Y >= X.