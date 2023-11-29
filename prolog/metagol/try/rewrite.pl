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
    metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]),
    check_recursion(Recursive,MaxN,Atom,Path),
    prove(Body,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).
prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
    N1 \== MaxN,
    Atom = [P,Args],
    bind_lower(P,A,FullSig,Sig1,Sig2),
    metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]),
    check_recursion(Recursive,MaxN,Atom,Path),
    check_new_metasub(Name,P,A,Subs,Prog1),
    succ(N1,N3),
    prove(Body,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs)|Prog1],Prog2).
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
        true).
%
bind_lower(P,A,FullSig,_Sig1,Sig2) :-
    nonvar(P), !,
    append(_,[sym(P,A,_)|Sig2],FullSig), !.
bind_lower(P,A,_FullSig,Sig1,Sig2) :-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U) ->
        U = 1, !;
        true).
%
%
user:term_expansion(metarule(Subs,Head,Body),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,_MetaBody,Asserts).
user:term_expansion(metarule(Name,Subs,Head,Body),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,_MetaBody,Asserts).
user:term_expansion((metarule(Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,MetaBody,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,MetaBody,Asserts).
%
metarule_asserts(Name,Subs,Head,Body1,MetaBody,[metagol:MRule]) :-
    Head = [P|_],
    is_recursive(Body1,P,Recursive), 
    add_path_to_body(Body1,Path,Body2), % 将Atom转换为p/4的形式，跳过@标记
    gen_metarule_id(Name,AssertName),
    forall(
        (member(p(P1,A1,_,_),Body2), ground(P1)),
        assert(type(P1,A1,compiled_pred))).
    (var(MetaBody) ->
        MRule = metarule(AssertName,Subs,Head,Body2,Recursive,Path);
        MRule = (metarule(AssertName,Subs,Head,Body2.Recursive,Path) :- MetaBody)).


% [[Q,A,C],[R,C,B]]
is_recursive([],_,false).
is_recursive([[Q|_]|_],P,true) :-
    Q == P, !.
is_recursive([_|T],P,Res) :-
    is_recursive(T,P,Res).
%
add_path_to_body([],_Path,[]).
add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest]) :-
    add_path_to_body(Atoms,Path,Rest).
add_path_to_body([[P|Args]|Atoms],Path,[p(P,A,Args,Path)|Rest]) :-
    length(Args,A),
    add_path_to_body(Atoms,Path,Rest).
%
gen_metarule_id(Name,Name) :-
    ground(Name), !.
gen_metarule_id(_Name,IdNext) :-
    current_predicate(metarule_next_id/1), !,
    metarule_next_id(Id),
    succ(Id,IdNext),
    set_option(metarule_next_id(IdNext)).
gen_metarule_id(_Name,1) :-
    set_option(metarule_next_id(2)).
%
check_recursion(false,_,_,_).
check_recursion(true,MaxN,Atom,Path) :-
    MaxN \== 1,
    \+memberchk(Atom,Path).
%
check_new_metasub(Name,P,A,Subs,Prog) :-
    memberchk(sub(Name,P,A,_),Prog), !,
    last(Subs,X),
    when(ground(X), \+memberchk(sub(Name,_P,A,Subs),Prog)).
check_new_metasub(_Name,_P,_A,_Subs,_Prog).