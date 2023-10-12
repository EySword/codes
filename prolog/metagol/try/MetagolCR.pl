parse(S):-parse(Q, S, []). 
parse(Q, X, X):-acceptor(Q). 

parse(Q, [C|X],Y):-delta1(Q,C,P), parse(P,X,Y). 
parse(Q, X, Y ):-delta2(Q,P,C), parse(P , X, [C|Y ]). 
parse(Q, X, Y ):-delta3(Q,P,R), parse(P,X,Z), parse(R,Z,Y).