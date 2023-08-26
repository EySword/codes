nature_number(0).
nature_number(s(X)) :- nature_number(X).

plus(0,X,X) :- nature_number(X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

even(0).
even(s(s(X))) :- even(X).

times(0,_Y,0).
times(s(X),Y,Z) :- plus(W,Y,Z), times(X,Y,W).

mod(X,Y,X) :- X<Y.
mod(X,Y,Z) :- plus(X1,Y,X), mod(X1,Y,Z).
