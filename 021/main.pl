merge([], L2, L2).


merge(L1, [], L1) :- L1 \= [].


merge([H1|T1], [H2|T2], [H1|T]) :-
    H1 =< H2,
    merge(T1, [H2|T2], T).


merge([H1|T1], [H2|T2], [H2|T]) :-
    H2 < H1,
    merge([H1|T1], T2, T).
