move(1,0).
move(0,1).
move(0,2).
move(2,0).
move(1,1).

legal((X,Y,_)):-
legal_an(X),
legal_an(Y).

legal_an((A,B)):- (A=:=0,B>=0,!);(B=:=0,A>=0,!);(A>=B,A>=0,B>=0).

update((X,Y,Q),Move,Statu):-(A,B)=X, (C,D)=Y,(E,F)=Move,
if_then_else(
Q=:=0, %Q == 0
(C1 is C+E, D1 is D+F, A1 is A-E, B1 is B-F, Statu=((A1,B1),(C1,D1),1)),
(C1 is C-E, D1 is D-F, A1 is A+E, B1 is B+F, Statu=((A1,B1),(C1,D1),0))
).

nextstatu(Statu,Statu1):- %
move(X,Y),
update(Statu,(X,Y),Statu1),
legal(Statu1).

if_then_else(P,Q,R):- call§,!,Q.
if_then_else(P,Q,R):- R.

nx(X,Y):- reverse(X,Y).

first(A,X):- append([A],_,X). %AX
last(B,X):- first(A,X),append([A],B,X). %

show(L):-if_then_else((length(L,X),X>0), %LX>0
(first(A,L),last(B,L),write('['),write(A),write(']'),nl,show(B)),
fail). %

findroad(X,Y,L):-
if_then_else(X=Y,
(write('------------'),nl,nx(L,LN),show(LN),nl), %nxL
(nextstatu(X,Z),not(member(Z,L)),findroad(Z,Y,[Z|L]))). %ZL
