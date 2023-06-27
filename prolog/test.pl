mortal(X):-person(X).
person(socrates).
person(plato).
person(aristotle).
mortal_report:-
write('Known morals are: '),nl, mortal(X),write(X),nl,fail.