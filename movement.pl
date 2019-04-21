% Norte, Sur, Este, Oeste, etc, dentro de la matriz y no pertenece a una NoDeseadasa(NoDeseadas).
direction8(X:Y,[X1,Y],NoDeseadas, N, M):- X1 is X-1, X1 > -1, X1 < N, not(member([X1,Y],NoDeseadas)).
direction8(X:Y,[X1,Y],NoDeseadas, N, M):- X1 is X+1, X1 > -1, X1 < N, not(member([X1,Y],NoDeseadas)).
direction8(X:Y,[X,Y1],NoDeseadas, N, M):- Y1 is Y-1, Y1 > -1, Y1 < M, not(member([X,Y1],NoDeseadas)).
direction8(X:Y,[X,Y1],NoDeseadas, N, M):- Y1 is Y+1, Y1 > -1, Y1 < M, not(member([X,Y1],NoDeseadas)).
direction8(X:Y,[X1,Y1],NoDeseadas, N, M):- Y1 is Y+1, X1 is X+1, Y1 > -1, X1 > -1, Y1 < M, X1 < N, not(member([X1,Y1],NoDeseadas)).
direction8(X:Y,[X1,Y1],NoDeseadas, N, M):- Y1 is Y+1, X1 is X-1, Y1 > -1, X1 > -1, Y1 < M, X1 < N, not(member([X1,Y1],NoDeseadas)).
direction8(X:Y,[X1,Y1],NoDeseadas, N, M):- Y1 is Y-1, X1 is X+1, Y1 > -1, X1 > -1, Y1 < M, X1 < N, not(member([X1,Y1],NoDeseadas)).
direction8(X:Y,[X1,Y1],NoDeseadas, N, M):- Y1 is Y-1, X1 is X-1, Y1 > -1, X1 > -1, Y1 < M, X1 < N, not(member([X1,Y1],NoDeseadas)).

% Dada una posicion devuelve las 8 posiciones validas alrededor de esta.
all_directions8([X0:Y0], R, L) :-
     n(N), m(M),
     findall([X,Y], direction8(X0:Y0,[X,Y], L, N, M), R).

ordered_union(L, [], R):- append(L, [], R), !.
ordered_union([], L, R):- append(L, [], R), !.
ordered_union(L, [H|T], R):-
    ((not(member(H,L)), append(L,[H],R1), ordered_union(R1, T, R));
    (member(H,L), ordered_union(L, T, R))).

nearest_child(Cola, Bebe, Bebes, Obstaculos, Corrales):-
    Cola = [H|T],
    H = [X,Y],

    ((   
        (not(member(H, Bebes));member(H, Corrales)),
        append([H], Obstaculos, NoDeseadas),
        all_directions8([X:Y],R1, NoDeseadas),
        ordered_union(T, R1, Cola1),
        nearest_child(Cola1, Bebe, Bebes, NoDeseadas,Corrales)
    );

    (
        member(H, Bebes),
        not(member(H, Corrales)),
        append(H, [], Bebe)
    )), !.

nearest_corral(Cola, Corral, Bebes, Corrales, Obstaculos):-
    Cola = [H|T],
    H = [X,Y],

    (
        (
            (not(member(H, Corrales));member(H, Bebes)),
            append([H], Obstaculos, NoDeseadas),
            all_directions8([X:Y],R1, NoDeseadas),
            ordered_union(T, R1, Cola1),
            nearest_corral(Cola1, Corral, Bebes, Corrales, NoDeseadas)
        );

        (
            member(H, Corrales),
            not(member(H, Bebes)),
            append(H, [], Corral)
        )
    ), !.

nearest_dirt(Cola, Dirt, Suciedades, Obstaculos):-
    Cola = [H|T],
    H = [X,Y],

    ((   
        not(member(H, Suciedades)),
        append([H], Obstaculos, NoDeseadas),
        all_directions8([X:Y],R1, NoDeseadas),
        ordered_union(T, R1, Cola1),
        nearest_dirt(Cola1, Dirt, Suciedades, NoDeseadas)
    );

    (
        member(H, Suciedades),
        append(H, [], Dirt)
    )), !.


% Norte, Sur, Este, Oeste, dentro de la matriz y no pertenece a una lista(List).
direction(X:Y,X1:Y,List, N, M):- X1 is X-1, X1 > -1, X1 < N, not(member([X1,Y],List)).
direction(X:Y,X1:Y,List, N, M):- X1 is X+1, X1 > -1, X1 < N, not(member([X1,Y],List)).
direction(X:Y,X:Y1,List, N, M):- Y1 is Y-1, Y1 > -1, Y1 < M, not(member([X,Y1],List)).
direction(X:Y,X:Y1,List, N, M):- Y1 is Y+1, Y1 > -1, Y1 < M, not(member([X,Y1],List)).
% Dada una posicion devuelve las 4 posiciones validas alrededor de esta.
all_directions([X0:Y0], R, L) :-
     n(N), m(M),
     findall(X:Y, direction(X0:Y0,X:Y, L, N, M), R).

% Caso base cuando hay solo una casilla
% (Head de la lista cuando esta viene con mas de 1 elementos)
all_valid_positions([X:Y], R, L):-  all_directions([X:Y], R, L), !.
% Caso base cuando solo hay un elemento en la lista
all_valid_positions([[X,Y]], R, L):-  all_directions([X:Y], R, L), !.

% L(Lista de corrales), R(Resultado)
all_valid_positions(L, R, _):-
    L = [H|T], H = [X,Y],
    all_valid_positions([X:Y], R1, L),
    all_valid_positions(T, R2, L),
    union(R1, R2, R), !.