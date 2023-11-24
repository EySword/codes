legal(_S,([],[])).
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
    C-CA =\= R-RA,
    C-CA =\= RA-R,
    legal_O((C,R),(CL,RL)).

not_in(_X,[]) :- !.
not_in(X,[F|L]) :- 
    X \= F,
    not_in(X,L).