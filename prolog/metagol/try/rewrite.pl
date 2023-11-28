learn(Pos,Neg) :-
    learn(Pos,Neg,Prog),
    pprint(Prog).
learn(Pos1,Neg1,Prog) :-
    setup,
    make_atoms(Pos1,Pos2),
    make_atoms(Neg1,Neg2),
    proveall(Pos2,Sig,Prog)

make_atoms(Atoms1,Atoms2) :-
    maplist(atom_to_list,Atoms1,Atoms3),
    maplist(make_atom,Atoms3,Atoms2).
make_atom([P|Args],p(P,A,Args,[])) :-
    length(Args,A).
%
proveall(Atoms,Sig,Prog) :-
    target_predicate(Atoms,P/A),
    format('% learning ~w\n',[P/A]),
    iterator(MaxN),
    format('% clause: ~d\n',[MaxN]),
    invented_symbols(MaxN,P/A,Sig),
    assert_sig_types(Sig),
    prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

target_predicate([p(P,A,_Args,[])|_],P/A).
%
iterator(N) :-
    min_clauses(MinN),
    max_clauses(MaxN),
    between(MinN,MaxN,N).
%
invent_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]) :-
    NumSymbols is MaxClauses-1,
    max_inv_preds(MaxInvPreds),
    M is min(NumSymbols,MaxInvPreds),
    findall(sym(Sym1,_Artiy,_Used1),
        (between(1,M,I), atomic_list_concat([P,'_',I],Sym1)),
        Sig).
%
assert_sig_types(Sig) :-
    forall((member(sym(P,A,_),Sig),\+type(P,A,head_pred)),
        assert(type(P,A,head_pred))).
%
prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
    deduce_Atom(Atom,FullSig,Prog1)%%%%%%%%%





%
%
deduce_atom(Atom,Sig,Prog) :-
    length(Prog,N),
    prove([Atom],Sig,_,N,N,N,Prog,Prog).
%
prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prpg).
prove([Atmo|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
    prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),

%
prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N.N,Prog,Prog) :-
    !,
    user:call(Atom).
prove_aux(p(P,A,Atgs,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
    nonvar(P),
    type(P,A,compiled_pred),!,
    compiled_pred_call(P,Args).
prove_aux(p(P,A,Atgs,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
    (nonvar(P) ->
        type(P,A,body_pred);
        true),
    body_pred_call(P,Args).
prove_aux(p(P,A,Args,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
    (var(P) ->
        true;
        (
            \+type(P,A,head_pred), !,
            type(P,A,ibk_head_pred)
        )),
    ibk([P|Args],Body,Path),
    prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
    N1 \== 0,
    Atom = [P|Args],
    select_lower(P,A,FullSig,Sig1,Sig2),
    member(sub(Name,P,A,Subs),Prog1),
    metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path])
%
%
setup :-
    options,
    head_preds, 
    body_preds,
    ibk_head_preds,
    compiled_preds.
%
options :-
    (current_predicate(min_clauses/1) ->
        true;
        (set_option(min_clauses(1)))),
    (current_predicate(max_clauses/1) ->
        true;
        (default(max_clauses(MaxN)),set_option(max_clauses(MaxN)))),
    (current_predicate(max_inv_preds/1) ->
        true;
        (max_clauses(MaxN),succ(MaxInv,MaxN),set_option(max_inv_preds(MaxInv)))).
set_option(Option) :- 
    functor(Option,Name,Arity),
    funtcor(Retract,Name,Arity),
    retractall(Retract),
    assert(Option).
%
head_preds :-
    retractall(type(_,_,head_pred)),
    forall(
        (user:head_pred(P/A), \+type(P,A,head_pred)),
        assert(type(P,A,head_pred))
    ).
body_preds :-
    retractall(type(_,_,body_pred)),
    retractall(body_pred_call(P,_)),
    findall(P/A,user:body_pred(P/A),S0),
    assert_body_preds(S0).
assert_body_preds(S1) :-
    forall(member(P/A,S1),
        (retractall(type(P,A,body_pred)),
        retractall(body_pred_call(P,_)),
        retractall(user:body_pred(P/A)),
        (current_predicate(P/A) -> (
            assert(type(P,A,body_pred)),
            assert(user:body_pred(P/A)),
            functor(Atom,P,A),
            Atom =.. [P|Args],
            assert((body_pred_call(P,Args) :- user:Atom))
            );
            format('% Waring: ~w does not exist\n',[P/A])
        )
    )).
%
ibk_head_preds :-
    findall(P/A,type(P,A,ibk_head_pred),S0),
    list_to_set(S0,S1),
    retractall(type(_,_,ibk_head_pred)),
    forall(
        member(P/A,S1),
        assert(type(P,A,ibk_head_pred))
    ).
%
compiled_preds :-
    findall(P/A,
        (type(P,A,compiled_pred), not(type(P,A,body_pred)), not(type(P,A,head_pred)), not(type(P,A,ibk_head_pred))),
        S0),
    list_to_set(S0,S1),
    retractall(type(_,_,compiled_pred)),
    retractall(compiled_pred_call(_,_)),
    forall(member(P/A,S1),(
        (current_predicate(P/A) -> (
            assert(type(P,A,compiled_pred)),
            functor(Atom,P,A),
            Atom =.. [P|Args],
            assert((compiled_pred_call(P,Args):-user:Atom))
        );
            format('% Waring: ~w does not exist\n',[P/A])
        ))
    ).
%
%
atom_to_list(Atom,AtomList) :- Atom =.. AtomList.
%
select_lower(P,A,FullSig,_Sig1,Sig2) :-
    nonvar(P),!,
    append(_,[sym(P,A,_)|Sig2],FullSig),!.
select_lower(P,A,_FullSig,Sig1,Sig2) :-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U) -> 
        !,fail;
        true
    ).
%
%
metarule_asserts(Name,Subs,Head,Body1,MetaBody,[metagol:MRule]) :-
    Head = [P|_],
    is_recrusive(Body1,P,Recrusive), 
    add_path_to_body(Body1,Path,Body2),





%
is_recrusive([],_,false).
is_recrusive([[Q|_]|_],P,true) :-
    Q == P, !.
is_recrusive([_|T],P,Res) :-
    is_recrusive(T,P,Res).
%
add_path_to_body([],_Path,[]).
add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest]) :-
    add_path_to_body(Atoms,Path,Rest).
add_path_to_body([[P|Args]|Atoms],Path,[p(P,A,Args,Path)|Rest]) :-
    length(Args,A),
    add_path_to_body(Atoms,Path,Rest).
