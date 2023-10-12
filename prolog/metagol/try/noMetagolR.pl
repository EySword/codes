parse(S,G1,G2) :- parse(s(0),S,[],G1,G2). 
parse(Q,X,X,G1,G2) :- abduce(acceptor(Q),G1,G2). 
parse(Q,[C|X],Y,G1,G2) :- Skolem(P), abduce(delta1(Q,C,P),G1,G3), parse(P,X,Y,G3,G2). 

abduce(X,G,G) :- member(X,G). 
abduce(X,G,[X|G]) :- not(member(X,G)). 

Skolem(s(0)). 
Skolem(s(1)).
