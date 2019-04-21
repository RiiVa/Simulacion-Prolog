:- dynamic n/1 , m/1.

setNM(N, M):-
    assert(n(N)),
    assert(n(N)).

win(ListChild, ListCorral, ListDirty):-
    length(ListDirty, R),
    R =:= 0,
    union(ListChild, ListCorral, ListUnion),
    length(ListUnion, T),
    length(ListChild,T1),
    T =:= T1,
    write("WIIIIIIIN, congratulation my bot"),!.


percentage(P, R) :- n(N), m(M), T is N*M, R is (100 * P/ T).

fail_test(ListDirty):-
    length(ListDirty,T),
    percentage(T,R),
    R > 60,
    write("FAIL"),nl.
    
    