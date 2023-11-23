move(2,0).
move(1,0).
move(1,1).
move(0,1).
move(0,2).

legal((X,Y,_)) :- 
    legal_an(X),
    legal_an(Y).

legal_an((A,B)) :- 
    (A=:=0,B>=0,!);
    (B=:=0,A>=0,!);
    (A>=B,A>=0,B>=0).

nextstatu(Statu,Statu1) :-
    move(X,Y),
    update(Statu,(X,Y),Statu1),
    legal(Statu1).

update((X,Y,Q),Move,Statu) :-
    (A,B)=X, (C,D)=Y, (E,F)=Move,
    (
        Q =:= 0 ->
        (
            C1 is C+E, D1 is D+F, A1 is A-E, B1 is B-F, Boot=1
        );(
            A1 is A+E, B1 is B+F, C1 is C-E, D1 is D-F, Boot=0
        )
    ),
    Statu=((A1,B1),(C1,D1),Boot).

findroad(Input) :-
    ((A,B),(C,D),P) = Input,
    C1 is A+C, D1 is B+D,
    findroad(Input,((0,0),(C1,D1),1),[((A,B),(C,D),P)]).
findroad(X,Y,L) :- % Y为目标状态
    X=Y -> (
        write('--------'),
        nl,
        reverse(L,LN),show(LN),nl
    );(
        nextstatu(X,Z),not(member(Z,L)),findroad(Z,Y,[Z|L])
    ).

show(L) :-
    length(L,X), X>0, 
    (first(A,L),last(B,L),write('['),write(A),write(']'),nl,show(B)).

first(A,L) :- append([A],_,L).
last(B,L) :- first(A,L), append([A],B,L).

