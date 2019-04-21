move_north(X:Y, [X1,Y],Obstaculos):- X1 is X - 1, n(N), X1 > -1, X1 < N, not(member([X1, Y],Obstaculos)).
move_south(X:Y, [X1,Y],Obstaculos):- X1 is X + 1, n(N), X1 > -1, X1 < N, not(member([X1, Y],Obstaculos)).
move_east(X:Y, [X,Y1],Obstaculos):- Y1 is Y + 1, m(M), Y1 > -1, Y1 < M, not(member([X, Y1],Obstaculos)).
move_west(X:Y, [X,Y1],Obstaculos):- Y1 is Y - 1, m(M), Y1 > -1, Y1 < M, not(member([X, Y1],Obstaculos)).
move_northwest(X:Y, [X1,Y1],Obstaculos):- X1 is X - 1, Y1 is Y - 1, X1 > - 1, Y1 > -1,  n(N), m(M), X1 < N, Y1 < M, not(member([X1, Y1],Obstaculos)).
move_northeast(X:Y, [X1,Y1],Obstaculos):- X1 is X - 1, Y1 is Y + 1, X1 > - 1, Y1 > -1,  n(N), m(M), X1 < N, Y1 < M, not(member([X1, Y1],Obstaculos)).
move_southwest(X:Y, [X1,Y1],Obstaculos):- X1 is X + 1, Y1 is Y - 1, X1 > - 1, Y1 > -1,  n(N), m(M), X1 < N, Y1 < M, not(member([X1, Y1],Obstaculos)).
move_southeast(X:Y, [X1,Y1],Obstaculos):- X1 is X + 1, Y1 is Y + 1, X1 > - 1, Y1 > -1,  n(N), m(M), X1 < N, Y1 < M, not(member([X1, Y1],Obstaculos)).

my_update([X], Pos , NewPos, R):-
    X==Pos,
    append([],[NewPos],R).
my_update([X], Pos , NewPos, R):-
    append([],[X],R).
my_update([X|List],Pos, NewPos, R):- 
    X == Pos,
    my_update(List, Pos ,NewPos, R1),
    append([NewPos], R1, R).
my_update([X|List], Pos, NewPos, R):-
    my_update(List, Pos, NewPos,R1),
    append([X], R1, R).

get_next_robot_move([RX, RY], [CX, CY], [NX, NY], Obstaculos):-
    (
        CX > RX, CY > RY, % south-east
        (
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos)));

    (
        CX > RX, CY =:= RY, % south
        (
            move_south(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos)));

    (
        CX > RX, CY < RY, % south-west
        (
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos)));

    (
        CX =:= RX, CY > RY, % east
        (
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos)));

    (
        CX =:= RX, CY < RY, % west
        (
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos)));

    (
        CX < RX, CY > RY, % north-east
        (
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos)));

    (
        CX < RX, CY =:= RY, % north
        (
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos)));

    (
        CX < RX, CY < RY, % north-west
        (
            move_northwest(RX:RY, [NX,NY], Obstaculos);
            move_north(RX:RY, [NX,NY], Obstaculos);
            move_west(RX:RY, [NX,NY], Obstaculos);
            move_northeast(RX:RY, [NX,NY], Obstaculos);
            move_east(RX:RY, [NX,NY], Obstaculos);
            move_southwest(RX:RY, [NX,NY], Obstaculos);
            move_southeast(RX:RY, [NX,NY], Obstaculos);
            move_south(RX:RY, [NX,NY], Obstaculos)));
            
        NX is RX, NY is RY.

count(_,[],0).
count(X,[X|T],N):-!,count(X,T,N1), N is N1+1.
count(X,[_|T],N):-count(X,T,N).

stupid_robot_agent(Robot, Obstaculos, Bebes, Corrales, Suciedades, [NextPos], N_Bebes, Suciedades1):-
    Robot = [[RX, RY]],
    (
        (
            member([RX,RY], Bebes),
            member([RX,RY], Corrales),
            count([RX,RY], Bebes, N), N > 1,
            nearest_corral(Robot, C, Bebes, Corrales, Obstaculos),
            get_next_robot_move([RX,RY], C, TempNextPos, Obstaculos),

            (
                (
                    member(TempNextPos, Bebes),
                    member(TempNextPos, Corrales),
                    get_next_robot_move(TempNextPos, C, NextPos, Obstaculos)
                );

                (
                    append(TempNextPos, [], NextPos)
                )
            ),

            my_update(Bebes, [RX,RY], NextPost, N_Bebes),
            append(Suciedades, [], Suciedades1)
        );

        (   
            
            nearest_child([[RX,RY]], B, Bebes, Obstaculos, Corrales),
            (
                (
                    member(B, Robot),
                    nearest_corral(Robot, C, Bebes, Corrales, Obstaculos),
                    get_next_robot_move([RX,RY], C, TempNextPos, Obstaculos),

                    (
                        (
                            member(TempNextPos, Corrales),
                            not(member(TempNextPos, Bebes)),
                            append(TempNextPos, [], NextPos)
                        );

                        (
                            get_next_robot_move(TempNextPos, C, NextPos, Obstaculos)
                        )
                    ),
                    my_update(Bebes, B, NextPos, N_Bebes),
                    append(Suciedades, [], Suciedades1)
                );
                (
                    get_next_robot_move([RX,RY], B, NextPos, Obstaculos),
                    append(Bebes, [], N_Bebes),
                    append(Suciedades, [], Suciedades1)
                )
            )
        );

        (
            nearest_dirt([[RX,RY]], D, Suciedades, Obstaculos),
            (
                (
                    D = [DX,DY],
                    DX =:= RX, DY =:= RY,
                    append(D, [], NextPos),
                    append(Bebes, [], N_Bebes),
                    delete(Suciedades, D, Suciedades1)
                );
                (

                    get_next_robot_move([RX,RY], D, NextPos, Obstaculos),
                    append(Bebes, [], N_Bebes),
                    append(Suciedades, [], Suciedades1)
                )
            )
        )

    ), !.


    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
% move_robot_to_corral(R, Robot, Obstaculos, Corrales):- move_robot_to_child(R, Robot, Obstaculos, Corrales).
