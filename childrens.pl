:- dynamic n/1, m/1, obstacle/2, corral/2, dirty/2.
:- include(movement).
:- include(printer).

assert_obstacles([E]):- E = [X,Y], assert(obstacle(X,Y)).
assert_obstacles([H|T]):- H = [X,Y], assert(obstacle(X,Y)), assert_obstacles(T).

assert_corrals([E]):- E = [X,Y], assert(corral(X,Y)).
assert_corrals([H|T]):- H = [X,Y], assert(corral(X,Y)), assert_corrals(T).

assert_dirties([E]):- E = [X,Y], assert(dirty(X,Y)).
assert_dirties([H|T]):- H = [X,Y], assert(dirty(X,Y)), assert_dirties(T).

move_north(X:Y, [X1,Y]):- X1 is X - 1, n(N), X1 < N, not(obstacle(X1, Y)).
move_south(X:Y, [X1,Y]):- X1 is X + 1, n(N), X1 < N, not(obstacle(X1, Y)).
move_east(X:Y, [X,Y1]):- Y1 is Y + 1, m(M), Y1 < M, not(obstacle(X, Y1)).
move_west(X:Y, [X,Y1]):- Y1 is Y - 1, m(M), Y1 < M, not(obstacle(X, Y1)).
move_northwest(X:Y, [X1,Y1]):- X1 is X - 1, Y1 is Y - 1, X1 > - 1, n(N), m(M), X1 < N, Y1 < M, not(obstacle(X1, Y1)).
move_northeast(X:Y, [X1,Y1]):- X1 is X - 1, Y1 is Y + 1, X1 > - 1, n(N), m(M), X1 < N, Y1 < M, not(obstacle(X1, Y1)).
move_southwest(X:Y, [X1,Y1]):- X1 is X + 1, Y1 is Y - 1, X1 > - 1, n(N), m(M), X1 < N, Y1 < M, not(obstacle(X1, Y1)).
move_southeast(X:Y, [X1,Y1]):- X1 is X + 1, Y1 is Y + 1, X1 > - 1, n(N), m(M), X1 < N, Y1 < M, not(obstacle(X1, Y1)).

get_next_child_move([RX, RY], [CX, CY], [NX, NY]):-
    (
        CX > RX, CY > RY, % south-east
        (
            move_southeast(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY])));

    (
        CX > RX, CY =:= RY, % south
        (
            move_south(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY])));

    (
        CX > RX, CY < RY, % south-west
        (
            move_southwest(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY])));

    (
        CX =:= RX, CY > RY, % east
        (
            move_east(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY])));

    (
        CX =:= RX, CY < RY, % west
        (
            move_west(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY])));

    (
        CX < RX, CY > RY, % north-east
        (
            move_northeast(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY])));

    (
        CX < RX, CY =:= RY, % north
        (
            move_north(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_northwest(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY])));

    (
        CX < RX, CY < RY, % north-west
        (
            move_northwest(RX:RY, [NX,NY]);
            move_north(RX:RY, [NX,NY]);
            move_west(RX:RY, [NX,NY]);
            move_northeast(RX:RY, [NX,NY]);
            move_east(RX:RY, [NX,NY]);
            move_southwest(RX:RY, [NX,NY]);
            move_southeast(RX:RY, [NX,NY]);
            move_south(RX:RY, [NX,NY]))).
    



% Dada una lista de corrales  (Corrals), genera todas las posiciones validas
% donde se puede poner un nuevo corral, y toma uno al azar usando sample
generate_next_corral(Corrals, R):-
    all_valid_positions(Corrals, R1, Corrals), sample(R1, R).

% Caso Base genera el primer corral de forma aleatoria.
generar_corrales(1, R):-
    n(N), m(M), X is random(N), Y is random(M),
    append([[X,Y]], [], R), !.

% Mientras Count sea mayor que 1, llama recursivo hasta tener el primer corral creado,
% despues a partir de ese corral genera otro corral y guardalo en la lista de corrales,
% y asi vas generando un corral nuevo a partir de los que ya tienes.
generar_corrales(Count, R):-
    Count1 is Count-1,
    generar_corrales(Count1, R1),
    generate_next_corral(R1, R2),
    R2 = X:Y, append(R1, [[X,Y]], R), !.

% Genera 1 obstaculo nuevo, y que su posicion no coincide con otro obstaculo,
% ni con un corral.
generar_obstaculo(Corrales, Obstaculos, R):-
    n(N), m(M), X is random(N), Y is random(M),
    not(member([X,Y], Corrales)),
    not(member([X,Y], Obstaculos)),
    append([[X,Y]], [], R), !.

% En caso de que el random genera una posicion invalida.
generar_obstaculo(A, B, C):-
    generar_obstaculo(A, B, C).

% Caso base de la recursividad, genera el primer obstaculo.
generar_obstaculos(1, Corrales, R):-
    n(N), m(M), X is random(N), Y is random(M),
    not(member([X,Y], Corrales)),
    append([[X,Y]], [], R), !.

% Genera todos los obstaculos.
generar_obstaculos(Count, Corrales, R):-
    Count > 1, Count1 is Count-1,
    generar_obstaculos(Count1, Corrales, Obstaculos),
    generar_obstaculo(Corrales, Obstaculos, Obstaculo),
    append(Obstaculo, Obstaculos, R), !.

% En caso de que el random genera una posicion invalida la primera vez.
generar_obstaculos(A, B, C):-
    generar_obstaculos(A, B, C).

% Genera 1 suciedad nueva, y que su posicion no coincide con un obstaculo, corral o suciedad.
generar_suciedad(Obstaculos, Corrales, Suciedades, R):-
    n(N), m(M), X is random(N), Y is random(M),
    not(member([X,Y],Obstaculos)),
    not(member([X,Y],Corrales)),
    not(member([X,Y],Suciedades)),
    append([[X,Y]], [], R), !.

% En caso de que el random genera una posicion invalida la primera vez.
generar_suciedad(A, B, C, D):-
    generar_suciedad(A, B, C, D).

% Caso base de la recursividad, genera la primera suciedad.
generar_suciedades(1, Obstaculos, Corrales, R):-
    n(N), m(M), X is random(N), Y is random(M),
    not(member([X,Y], Obstaculos)),
    not(member([X,Y], Corrales)),
    append([[X,Y]],[], R).

% Genera toda la suciedad.
generar_suciedades(Count, Obstaculos, Corrales, R):-
    Count > 1, Count1 is Count-1,
    generar_suciedades(Count1, Obstaculos, Corrales, Suciedades),
    generar_suciedad(Obstaculos, Corrales, Suciedades, Suciedad),
    append(Suciedad, Suciedades, R), !.

generar_suciedades(A, B, C, D):-
    generar_suciedades(A, B, C, D).

% Define globalmente las dimensiones del tablero y va generando las casillas de cada tipo
generar_tablero(N, M, Cpct, Opct, Spct, Obstaculos, Corrales, Suciedades, Bebes, Robot):-
    assert(n(N)), assert(m(M)),
    percentage(Cpct, Ccount), write("Corrales -> "), writeln(Ccount),
    generar_corrales(Ccount, Corrales), !,                                   %genera los corrales
    percentage(Opct, Ocount), write("Obstaculos -> "), writeln(Ocount),                         
    generar_obstaculos(Ocount, Corrales, Obstaculos), !,                     %genera los obstaculos
    generar_obstaculos(Ccount, Obstaculos, Bebes), !,                        %genera los bebes
    generar_obstaculos(1, Obstaculos, Robot), !,
    percentage(Spct, Scount), write("Suciedades -> "), writeln(Scount), nl,                  
    generar_suciedades(Scount, Obstaculos, Corrales, Suciedades), !,         %genera las suciedades
    
    assert_obstacles(Obstaculos),
    assert_corrals(Corrales),
    assert_dirties(Suciedades),

    nearest_child(Robot, B, Bebes, Obstaculos),
    writeln(B),
    print_general(N,M,Suciedades,Obstaculos,Bebes,Robot,Corrales), !.


sample(L, R) :- length(L, Len), random(0, Len, Random), nth0(Random, L, R).
percentage(P, R) :- n(N), m(M), T is N*M, R1 is (P / 100 * T), round(R1, R).


% start :-
%     generar_tablero(15,15,5,5,5).
