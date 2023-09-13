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

delete([],X,[]).
delete([X|Xs],X,Y) :- delete(Xs,X,Y).
delete([X|Xs],Z,[X|Y]) :- delete(Xs,Z,Y).

select(X,[X|Xs],Xs).
select(X,[Z|Zs],[Z|Y]) :- select(X,Zs,Y).

insert(X,Ys,Zs) :- select(X,Zs,Ys).

sort(Xs,Ys) :- permutation(Xs,Ys), write(Ys), ordered(Ys).

ordered([X]).
ordered([X,Y|Z]) :- X=<Y, ordered([Y|Z]).

permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys), permutation(Ys,Zs).
permutation([],[]).

binary_tree(void).
binary_tree(tree(Element,Left,Right)) :- binary_tree(Left), binary_tree(Right).

tree_member(X,tree(X,Left,Right)).
tree_member(X,tree(Element,Left,Right)) :- tree_member(X,Left).
tree_member(X,tree(Element,Left,Right)) :- tree_member(X,Right).