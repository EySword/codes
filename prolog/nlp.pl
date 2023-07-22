sentence :- %（句子）
    nounphrase, verbphrase.

nounphrase :- %（名词短语）
    determiner, nounexpression.
nounphrase :- %（名词短语）
    nounexpression.

nounexpression :-
    noun.
nounexpression :-
    adjective, %（形容词）
    nounexpression.

verbphrase :- %（动词短语）
    verb, nounphrase.
determiner :- %（限定词）
    the | a.
noun :- %（名词）
    dog | bone | mouse | cat.
verb :- %（动词）
    ate | chases.
adjective :-
    big | brown | lazy.


% 差异表

sentence(S) :- 
    nounphrase(S-S1),
    verbphrase(S1-[]).

sentence2 --> nounphrase, verbphrase.

noun([dog|X]-X).
noun([cat|X]-X).
noun([mouse|X]-X).
noun --> [dog].
noun --> [cat].

verb([ate|X]-X).
verb([chases|X]-X).
verb --> [chases].

adjective([big|X]-X).
adjective([brown|X]-X).
adjective([lazy|X]-X).

determiner([the|X]-X).
determiner([a|X]-X).
determiner --> [the].
determiner --> [a].

nounphrase(NP-X):-
    determiner(NP-S1),
    nounexpression(S1-X).
nounphrase --> determiner, nounexpression.

nounphrase(NP-X):-
    nounexpression(NP-X).
nounphrase --> nounexpression.

nounexpression(NE-X):-
    noun(NE-X).
nounexpression --> noun.

nounexpression(NE-X):-
    adjective(NE-S1),
    nounexpression(S1-X).

verbphrase(VP-X):-
    verb(VP-S1),
    nounphrase(S1-X).
verbphrase --> verb, nounphrase.
