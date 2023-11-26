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
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
