target_predicate([p(P,A,_Args,[])|_],P/A).

make_atoms(Atoms1,Atoms2):-
    maplist(atom_to_list,Atoms1,Atoms3), % Atoms3:[[P,a,b,c,...]|...]
    maplist(make_atom,Atoms3,Atoms2).
make_atom([P|Args],p(P,A,Args,[])):-
    length(Args,A).
atom_to_list(Atom,AtomList):-
    Atom =..AtomList.

% :-
% Pos = [
%     f([1,2,3,4,5,6,7,8,9,10],[2,4,5,6,8,10]),
%     f([5,6,7,8,9,5],[5,8,5]),
%     f([10,20,30],[10,20,30]),
%     f([7],[])
% ],
% make_atoms(Pos,Pos2),
% target_predicate(Pos2,Y),
% write(Y).
:- op(950,fx,'@').

runTest(X,Y) :-
    X = '@'(a) ->
    Y = true;
    Y = false.