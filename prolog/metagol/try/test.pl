parse(S) :- parse(q0,S,[]). 
parse(Q, [], []) :- acceptor(Q). 
parse(Q, [C|X],Y) :- delta1(Q,C,P), parse(P,X,Y).

acceptor(q0).
delta1(q0, 0,q0).
delta1(q0, 1,q1).
delta1(q1, 0,q1).
delta1(q1, 1,q0).

q0([], []).
q0([0|A],B):-q0(A, B).
q0([1|A],B):-q1(A, B).
q1([0|A],B):-q1(A, B).
q1([1|A],B):-q0(A, B).

