:- use_module('test2').

metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

% term_expansion(metarule(Subs,Head,Body),metarule(Subs,Head,Body,'rec')) :-
%     write('=====\n').