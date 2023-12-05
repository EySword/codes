metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

user:term_expansion(metarule(Subs,Head,Body),metarule('name',Subs,Head,Body,'rec','out')).