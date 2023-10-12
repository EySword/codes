% MetagolR
parse(S,G1,G2,S1,S2,K1,K2) :- parse(s(0),S,[],G1,G2,S1,S2,K1,K2). 
parse(Q,X,X,G1,G2,S,S,K1,K2) :- abduce(acceptor(Q),G1,G2,K1,K2). 
parse(Q,[C|X],Y,G1,G2,S1,S2,K1,K2) :- 
    Skolem(P,S1,S3), abduce(delta1(Q,C,P),G1,G3,K3,K2), parse(P,X,Y,G3,G2,S3,S2,K3,K2). 

abduce(X,G,G,K,K) :- member(X,G). 
abduce(X,G,[X|G],s(K),K) :- not(member(X,G)). 

Skolem(s(N),[s(Pre)|SkolemConsts],[s(N),s(Pre)|SkolemConsts]):- N is Pre+1. 
Skolem(S,SkolemConsts,SkolemConsts):-member(S,SkolemConsts).