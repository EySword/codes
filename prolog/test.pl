% :- use_module('test2').

% metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

term_expansion(metarule(Subs,Head,Body),metarule('22',Subs,Head,Body)).


%     write('=====\n').