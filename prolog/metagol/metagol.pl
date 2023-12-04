%% This file is a copyrighted under the BSD 3-clause licence, details of which can be found in the root directory.

:- module(metagol,[learn/2,learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).



% 这6个谓词是动态的
:- dynamic
    ibk/3,
    functional/0,
    user:head_pred/1,
    user:body_pred/1,
    body_pred_call/2,
    compiled_pred_call/2.


% 最大子句的长度为10

default(max_clauses(10)).



% learn为MIL的入口，learn/2输出学到的程序，learn/3将学到的程序储存在Prog中

learn(Pos,Neg):-
    learn(Pos,Neg,Prog),
    % write('next is mainProg:\n'),
    pprint(Prog).

learn(Pos1,Neg1,Prog):-
    setup,
    make_atoms(Pos1,Pos2), % Pos2的结构为[p(谓词名称,元数,[参数],[])|...]
    % format('It\'s Pos2:\n~w\n========\n',[Pos2]),
    make_atoms(Neg1,Neg2),
    proveall(Pos2,Sig,Prog),
    % writeChk('Prog',Prog),
    nproveall(Neg2,Sig,Prog),
    ground(Prog),
    check_functional(Pos2,Sig,Prog).

learn(_,_,_):-!,
    writeln('% unable to learn a solution'),
    false.


% proveall/3 和 prove_examples/7 用于生成和验证正例的程序

proveall(Atoms,Sig,Prog):- % Atoms为[p(P,A,[Args],[])|...]
    target_predicate(Atoms,P/A), % Atoms中谓词的形式是统一的target，这里提取出列这个统一的形式为P/A
    format('% learning ~w\n',[P/A]),
    iterator(MaxN),  %  between(Min,Max,MaxN)，一个从MinN到MaxN的迭代生成器
    format('% clauses: ~d\n',[MaxN]),
    invented_symbols(MaxN,P/A,Sig), % Sig:[sym(P, A, _),sym(P_1,A,_),...]
    % writeChk('Atoms',Atoms),
    % writeChk('Sig',Sig),
    assert_sig_types(Sig), % 将Sig内的sym/3标记为type:head_pred
    prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog). % 输入的Atoms为空，返回空的Prog
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    deduce_atom(Atom,FullSig,Prog1),!, % Atom为p(P,A,[Args],[])
    % 如果这里Atom在已有列表里则剪枝？
    check_functional([Atom],Sig,Prog1),
    prove_examples(Atoms,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove([Atom],FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    check_functional([Atom],Sig,Prog3),
    prove_examples(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

% deduce_atom/3 用于推断一个原子是否在给定的程序中。
% 在Sig和Prog的环境下推导Atom？
deduce_atom(Atom,Sig,Prog):- % Atom为p(P,A,[Args],[])
    length(Prog,N),
    prove([Atom],Sig,_,N,N,N,Prog,Prog).

% prove/8 用于验证一个原子是否能够通过给定的程序中的规则来推导出。

prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):- 
    prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3), % Atom为p(P,A,[Args],[])
    prove(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):- !,
    % write('>>>aux1<<<\n'),
    user:call(Atom). % 如果Atom符合'@'标记的形式，则直接call

prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):- % compiled_pred情况下N和Prog直接回传
    nonvar(P), % P需要绑定具体的值
    type(P,A,compiled_pred),!, % 如果P/A已经是compiles_pred，则不再重复寻找?
    % write('>>>aux2<<<\n'),
    compiled_pred_call(P,Args). % 运行P/A

prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):- % body_pred情况下N和Prog直接回传
    (nonvar(P) -> type(P,A,body_pred); true), % 没太懂true的用途
    % write('>>>aux3<<<\n'),
    body_pred_call(P,Args). % 测试是否满足已有的BK

prove_aux(p(P,A,Args,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    % 先判断是否是ibk_head_pred
    (var(P) -> true; (\+ type(P,A,head_pred), !, type(P,A,ibk_head_pred))), 
    ibk([P|Args],Body,Path), % 调用ibk
    % write('>>>aux4<<<\n'),
    prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    N1 \== 0,
    Atom = [P|Args],
    select_lower(P,A,FullSig,Sig1,Sig2), % 在FullSig中寻找sym(P,A,U)，Sig2为寻找到后，后面的列表
    member(sub(Name,P,A,Subs),Prog1), % 好像一开始Prog1是空的，可能这里会失败？
    metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]), % Atom为形式化的metagol:MRule??
    check_recursion(Recursive,MaxN,Atom,Path),
    % write('>>>aux5<<<\n'),
    prove(Body,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    N1 \== MaxN,
    % format('N1:~w, MaxN:~w\n',[N1,MaxN]),
    Atom = [P|Args],
    bind_lower(P,A,FullSig,Sig1,Sig2),
    % format('>> bind lower: \n~w/~w: FullSig: ~w, Sig1: ~w, Sig2: ~w.\n',[P,A,FullSig,Sig1,Sig2]),
    metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]), % ??
    check_recursion(Recursive,MaxN,Atom,Path),
    check_new_metasub(Name,P,A,Subs,Prog1),
    succ(N1,N3),
    % write('>>>aux6<<<\n'),
    prove(Body,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs)|Prog1],Prog2).

nproveall(Atoms,Sig,Prog):-
    forall(member(Atom,Atoms), \+ deduce_atom(Atom,Sig,Prog)).

% 将输入的正例Pos1先转换为列表，再将每一个的形式从[P|Args]转换成p(P,A,Args,[]). 其中Args为[a,b,c,...]
make_atoms(Atoms1,Atoms2):-
    maplist(atom_to_list,Atoms1,Atoms3), % Atoms3:[[P,a,b,c,...]|...]
    maplist(make_atom,Atoms3,Atoms2).
make_atom([P|Args],p(P,A,Args,[])):-
    length(Args,A).

% check_functional/3 用于检查程序的功能性

check_functional(Atoms,Sig,Prog):-
    (functional ->
        forall(member(Atom1,Atoms),
        \+ (
            make_atom(Atom2,Atom1), % Atom2:[P|Args], Atom1:[p/4]
            user:func_test(Atom2,TestAtom2,Condition),
            make_atom(TestAtom2,TestAtom1),
            % writeChk('Atom2',Atom2),
            % writeChk('TestAtom2',Atom2),
            % write('Condition',Condition),
            deduce_atom(TestAtom1,Sig,Prog),
            \+ call(Condition)));
        true).

% check_recursion/4 用于检查递归规则
check_recursion(false,_,_,_).
check_recursion(true,MaxN,Atom,Path):-
    MaxN \== 1, % need at least two clauses if we are using recursion
    \+memberchk(Atom,Path).

% P不是变量则在FullSig中找，P是变量则在Sig1中找
select_lower(P,A,FullSig,_Sig1,Sig2):-
    nonvar(P),!,
    append(_,[sym(P,A,_)|Sig2],FullSig),!. % 找sym(P,A,_)在FullSig中的位置
select_lower(P,A,_FullSig,Sig1,Sig2):-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U)-> !,fail;true ).

bind_lower(P,A,FullSig,_Sig1,Sig2):- % 判断sym[P,A,_]是否在FullSig中
    nonvar(P),!,
    append(_,[sym(P,A,_)|Sig2],FullSig),!.
bind_lower(P,A,_FullSig,Sig1,Sig2):-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U)-> U = 1,!;true).

check_new_metasub(Name,P,A,Subs,Prog):-
    memberchk(sub(Name,P,A,_),Prog),!,
    last(Subs,X),
    when(ground(X), \+memberchk(sub(Name,_P,A,Subs),Prog)).
check_new_metasub(_Name,_P,_A,_Subs,_Prog).

assert_sig_types(Sig):-
    forall((member(sym(P,A,_),Sig),\+type(P,A,head_pred)),
        assert(type(P,A,head_pred))).

% head_preds 的目的是初始化头部谓词的设置。
% 它会检查用户定义的头部谓词，并为每个符合条件的头部谓词添加一个标记，以便在后续的学习过程中使用
% \+type(P,A,head_pred)：如果谓词 P/A 没有被标记为 head_pred 类型，即条件为真（true），否则返回假（false）

head_preds:-
    %% remove old invented predicates (not that it really matters)
    retractall(type(_,_,head_pred)),
    forall((user:head_pred(P/A),\+type(P,A,head_pred)),
        assert(type(P,A,head_pred))).

%  findall/3 查找用户定义的体谓词，并将它们存储在S0列表中

body_preds:-
    retractall(type(_,_,body_pred)),
    retractall(body_pred_call(P,_)),
    findall(P/A,user:body_pred(P/A),S0),
    % write(S0),
    assert_body_preds(S0).

% 这个函数就是由S1=[P/A,P/A...]生成：
% type(P,A,body_pred)、
% user:body_pred(P/A)、
% body_pred_call(P,Args):-user:Atom
assert_body_preds(S1):-
    forall(member(P/A,S1),(
        retractall(type(P,A,body_pred)),
        retractall(body_pred_call(P,_)),
        retractall(user:body_pred(P/A)),
        (current_predicate(P/A) -> (
            assert(type(P,A,body_pred)),
            assert(user:body_pred(P/A)),
            functor(Atom,P,A),
            Atom =.. [P|Args],
            % format('body Atom is: ~w\n',[Atom]),
            assert((body_pred_call(P,Args):-user:Atom))
        );
            format('% WARNING: ~w does not exist\n',[P/A])
        )
    )).

% 看起来是去重了
ibk_head_preds:-
    findall(P/A,type(P,A,ibk_head_pred),S0),
    list_to_set(S0,S1),
    retractall(type(_,_,ibk_head_pred)),
    forall(member(P/A,S1),
        assert(type(P,A,ibk_head_pred))
    ).

compiled_preds:-
    findall(P/A,(type(P,A,compiled_pred), not(type(P,A,body_pred)), not(type(P,A,head_pred)), not(type(P,A,ibk_head_pred))),S0),
    list_to_set(S0,S1),
    retractall(type(_,_,compiled_pred)),
    retractall(compiled_pred_call(_,_)),
    forall(member(P/A,S1),(
        (current_predicate(P/A) -> (
            assert(type(P,A,compiled_pred)),
            functor(Atom,P,A),
            Atom =..[P|Args],
            assert((compiled_pred_call(P,Args):-user:Atom)) % 这里定义好了怎样运行P/A
        );
            format('% WARNING: ~w does not exist\n',[P/A])
        )
    )).

options:-
    (current_predicate(min_clauses/1) -> true; (set_option(min_clauses(1)))),
    (current_predicate(max_clauses/1) -> true; (default(max_clauses(MaxN)),set_option(max_clauses(MaxN)))),
    (current_predicate(max_inv_preds/1) -> true; (max_clauses(MaxN),succ(MaxInv,MaxN),set_option(max_inv_preds(MaxInv)))).

set_option(Option):- %先删除所有和Option同名的谓词，再生成新的Option谓词
    functor(Option,Name,Arity),
    functor(Retract,Name,Arity),
    retractall(Retract),
    assert(Option).


% head/body_preds：这两个谓词用于初始化头部谓词和体谓词。
% 它们会根据用户定义的谓词规则，将谓词和它们的参数类型添加到程序中。
% 这些信息在学习过程中用于生成元规则和验证学习到的程序。

setup:- % setup初始化MIL的框架
    options, % option用于设置学习参数（最小子句数、最大子句数以及最大发明的预测谓词数）
    head_preds, % 生成type（P，A，head_pred）样式的谓词头部
    body_preds, % 生成 type(P,A,body_pred)、user:body_pred(P/A)、body_pred_call(P,Args):-user:Atom
    % 下面两个看起来就是去重
    ibk_head_preds, % IBK 是用于学习的背景知识，它描述了先验知识中的一些关系
    compiled_preds. % compiled_preds：初始化编译谓词，即已经编译的谓词




iterator(N):-
    min_clauses(MinN),
    max_clauses(MaxN),
    between(MinN,MaxN,N).

target_predicate([p(P,A,_Args,[])|_],P/A).

%% target_predicates(Atoms, Preds2):-
%%     findall(P/A, member([p(inv,P,A,_Args,_Atom,[])],Atoms), Preds1),
%%     list_to_set(Preds1,Preds2).

invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]):- % 输出是一个由sym/3元素组成的列表
% [sym(P,A,_U),sym(P_1,_A,_U),sym(P_2,_A,_U)...]
    NumSymbols is MaxClauses-1, % 要生成的符号的数量
    max_inv_preds(MaxInvPreds), % 查询最大发明谓词数
    M is min(NumSymbols,MaxInvPreds), % 要生成的符号数量
    findall(sym(Sym1,_Artiy,_Used1),(between(1,M,I),atomic_list_concat([P,'_',I],Sym1)),Sig).


% pprint/1 用于打印学习到的程序。

pprint(Prog1):-
    reverse(Prog1,Prog3),
    maplist(metasub_to_clause,Prog3,Prog2),
    maplist(pprint_clause,Prog2).

pprint_clause(C):-
    numbervars(C,0,_),
    format('~q.~n',[C]).

metasub_to_clause(sub(Name,_,_,Subs),Clause2):-
    metarule(Name,Subs,HeadList,BodyAsList1,_,_),
    add_path_to_body(BodyAsList3,_,BodyAsList1),
    include(no_ordering,BodyAsList3,BodyAsList2),
    maplist(atom_to_list,ClauseAsList,[HeadList|BodyAsList2]),
    list_to_clause(ClauseAsList,Clause1),
    (Clause1 = (H,T) -> Clause2=(H:-T); Clause2=Clause1).

no_ordering(H):-
    H\='@'(_).

list_to_clause([Atom],Atom):-!.
list_to_clause([Atom|T1],(Atom,T2)):-!,
    list_to_clause(T1,T2).

atom_to_list(Atom,AtomList):-
    Atom =..AtomList.

%% build the internal metarule clauses
% 上面调用metarule/6，这里生成metarule/6
user:term_expansion(metarule(Subs,Head,Body),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,_MetaBody,Asserts).
user:term_expansion(metarule(Name,Subs,Head,Body),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,_MetaBody,Asserts).
user:term_expansion((metarule(Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,MetaBody,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,MetaBody,Asserts).

%% metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
% 通过这个，把：
% Name变成序号
% Body从Atom的形式转换为p/4
% MetaBody根据是否存在生成或不生成
% 返回metagol模块的metarule/6
metarule_asserts(Name,Subs,Head,Body1,MetaBody,[metagol:MRule]):-
    Head = [P|_],
    is_recursive(Body1,P,Recursive), % 判断头部P是否在Body中（递归）, 返回为true/false
    add_path_to_body(Body1,Path,Body2), % 将Atom形式转化为p/4, @标记的不变
    gen_metarule_id(Name,AssertName), % 生成序号
    %% very hacky - I assert that all ground body predicates are compiled
    %% I filter these in the setup call
    % 把Body2里的每一个p/4打上标记：compiled_pred
    forall((member(p(P1,A1,_,_),Body2), ground(P1)), assert(type(P1,A1,compiled_pred))), 
    (var(MetaBody) ->
        MRule = metarule(AssertName,Subs,Head,Body2,Recursive,Path);
        MRule = (metarule(AssertName,Subs,Head,Body2,Recursive,Path):-MetaBody)).
%% metarule(id:1, 
    % Subs:[P,Q,R], 
    % Head:[P,A,B], 
    % Body2:[p(Q,2,[A,C],Path),p(R,2,[C,B],Path)],
    % true/false,Path)


% 在运行ibk的时候，先调用ibk_asserts
user:term_expansion((ibk(Head,Body):-IbkBody),(ibk(Head,Body):-IbkBody)):-
    ibk_asserts(Head,Body,IbkBody,[]).

user:term_expansion(ibk(Head,Body),ibk(Head,Body)):-
    ibk_asserts(Head,Body,false,[]).

ibk_asserts(Head,Body1,IbkBody,[]):-
    Head = [P0|Args1],
    length(Args1,A0),
    assert(type(P0,A0,ibk_head_pred)),
    add_path_to_body(Body1,Path,Body2),
    (IbkBody == false -> assert(ibk(Head,Body2,Path)); assert((ibk(Head,Body2,Path):-IbkBody))),
    %% very hacky - I assert that all ground body predicates are compiled
    %% I filter these in the setup call
    forall((member(p(P1,A1,_,_),Body2), ground(P1)), assert(type(P1,A1,compiled_pred))).

% 判断是否递归
is_recursive([],_,false).
is_recursive([[Q|_]|_],P,true):-
    Q==P,!.
is_recursive([_|T],P,Res):-
    is_recursive(T,P,Res).

add_path_to_body([],_Path,[]).
add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest]):- % 如果已被标记为‘@’那么跳过
    add_path_to_body(Atoms,Path,Rest).
add_path_to_body([[P|Args]|Atoms],Path,[p(P,A,Args,Path)|Rest]):- % 将Atom转换为p/4的形式
    length(Args,A),
    add_path_to_body(Atoms,Path,Rest).

% 但是这里排序是1,3,4,5,6... ？
gen_metarule_id(Name,Name):- % Name是变量则失败
    ground(Name),!.
gen_metarule_id(_Name,IdNext):-
    current_predicate(metarule_next_id/1),!, % 判断是否存在metarule_next_id/1
    metarule_next_id(Id),
    succ(Id,IdNext),
    set_option(metarule_next_id(IdNext)). % 将metarule_next_id加1
gen_metarule_id(_Name,1):- % 第一次运行进入这里，返回当前
    set_option(metarule_next_id(2)).

learn_seq(Seq,Prog):-
    maplist(learn_task,Seq,Progs),
    flatten(Progs,Prog).

learn_task(Pos/Neg,Prog1):-
    learn(Pos,Neg,Prog1),!,
    maplist(metasub_to_clause,Prog1,Prog2),
    forall(member(Clause,Prog2),assert(user:Clause)),
    findall(P/A,(member(sub(_Name,P,A,_Subs),Prog1)),Preds),!,
    assert_body_preds(Preds).
learn_task(_,[]).

writeChk(Name,Aim) :-
    format('>>> ~w -----------------------\n~w\n^^^^^^^^^^^^^^^^^^^^^^^^^^^\n',[Name,Aim]).