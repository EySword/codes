board(8).

legal(_S,([],[])) :- !.
legal(S,BL) :- % S=(C,R), BL=([C],[R])
    legal_R(S,BL),
    legal_C(S,BL),
    legal_O(S,BL).

legal_R((_C,R),(_CL,RL)) :-
    not_in(R,RL).
legal_C((C,_R),(CL,_RL)) :-
    not_in(C,CL).
legal_O(_S,([],[])) :- !.
legal_O((C,R),([CA|CL],[RA|RL])) :-
    C-CA =\= RA-R,
    C-CA =\= R-RA,
    legal_O((C,R),(CL,RL)).

play(N,BL,BL) :- board(Max), N is Max+1, !.
play(CNumber,BL,Result) :- 
    board(Max),
    getStation(CNumber,C,R),
    legal((C,R),BL),
    CNumber =< Max,
    (CL,RL) = BL,
    Next is CNumber+1,
    play(Next,([C|CL],[R|RL]),Result).


count(N) :-
    findall(BL, play(1,([],[]),(_,BL)),R),
    % remove_duplicates(R,Rs),
    length(R,N).

remove_duplicates(List, Unique) :-
    sort(List, Unique).

getStation(CNumber,C,R) :-
    board(Max),
    generateBoard(CNumber,Max,BC),
    member(C,BC),
    generateBoard(1,Max,BR),
    member(R,BR).

generateBoard(Start,End,L) :- 
    findall(Number, between(Start,End,Number), L).


not_in(_X,[]) :- !.
not_in(X,[F|L]) :- 
    X \= F,
    not_in(X,L).