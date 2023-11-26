prove([],_FullSig,_sig,_MaxN,N,N,Prog,Prog).

prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    prove([Atoms],FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).


prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):- 
    !,
    user:call(Atom).

prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-
    nonvar(P),
    type(P,A,compiled_pred),
    !,
    compiled_pred_call(P,Args).