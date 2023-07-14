% 阶乘

factorial(1,1).
factorial(N,X) :- 
    N > 1,
    NN is N-1,
    factorial(NN,F),
    X is N*F.

factorial_2(N,X) :- factorial_2(N,1,X).
factorial_2(1,F,F).
factorial_2(N,F,X) :- 
    N>1,
    FF is N*F,
    NN is N-1,
    factorial_2(NN,FF,X).

% 列表元素倒置

naive_reverse([],[]).
naive_reverse([H|T],X) :-
    naive_reverse(T,Temp),
    append(Temp,[H],X).

reverse(OldList,X) :- reverse(OldList,[],X).
reverse([],X,X).
reverse([H|T],Temp,X) :-
    reverse(T,[H|Temp],X).
    