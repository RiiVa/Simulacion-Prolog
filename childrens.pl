:- dynamic n/1, m/1.
:- include(movement).
:- include(paint).
:- include(robot).

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

generar_suciedades(A, B, C, D):- generar_suciedades(A, B, C, D).

% Define globalmente las dimensiones del tablero y va generando las casillas de cada tipo
generar_tablero(N, M, Cpct, Opct, Spct, Obstaculos, Corrales, Suciedades, Bebes, Robot):-
    assert(n(N)), assert(m(M)),
    percentage(Cpct, Ccount), generar_corrales(Ccount, Corrales), !,                                   %genera los corrales
    percentage(Opct, Ocount), generar_obstaculos(Ocount, Corrales, Obstaculos), !,                     %genera los obstaculos
    generar_suciedades(Ccount, Obstaculos, Corrales, Bebes), !,                                        %genera los bebes
    generar_obstaculos(1, Obstaculos, Robot), !,
    percentage(Spct, Scount), generar_suciedades(Scount, Obstaculos, Corrales, Suciedades), !,         %genera las suciedades
    print_general(N,M,Suciedades,Obstaculos,Bebes,Robot,Corrales),!.
    % stupid_robot_agent(Robot, Obstaculos, Bebes, Corrales, Robot1, Bebes1),
    % print_general(N,M,Suciedades,Obstaculos,Bebes1,Robot1,Corrales),
    % stupid_robot_agent(Robot1, Obstaculos, Bebes1, Corrales, Robot2, Bebes2),
    % print_general(N,M,Suciedades,Obstaculos,Bebes2,Robot2,Corrales),
    % stupid_robot_agent(Robot2, Obstaculos, Bebes2, Corrales, Robot3, Bebes3),
    % print_general(N,M,Suciedades,Obstaculos,Bebes3,Robot3,Corrales),
    % stupid_robot_agent(Robot3, Obstaculos, Bebes3, Corrales, Robot4, Bebes4),
    % print_general(N,M,Suciedades,Obstaculos,Bebes4,Robot4,Corrales), !.

sample(L, R) :- length(L, Len), random(0, Len, Random), nth0(Random, L, R).
percentage(P, R) :- n(N), m(M), T is N*M, R1 is (P / 100 * T), round(R1, R).

