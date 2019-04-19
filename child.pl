:- dynamic n/1 , m/1.
:-include(paint).


validate_child([PosX|PosY],[X|Y]) :- 
    abs(PosX - X) < 2 ,
    abs(PosY - Y) < 2 .

validate_pos([X,Y], N, M):-
     X < N,
     X > -1,
     Y > -1,
     Y < M.

child_area_count(Pos, ListChild, Count) :-
    append([],[[0,1],[1,0],[-1,0],[0,-1],[1,1],[1,-1],[-1,1],[-1,-1]],T),
    my_map(T, Pos, Result),

    child_area_count_2(Result, ListChild, Count).
    
child_area_count_2([X], ListChild, R):- 
    any_child(X, ListChild, T),
    R is T.

child_area_count_2([X|Y],ListChild , R) :-
    child_area_count_2(Y, ListChild, R1),
    any_child(X , ListChild, T),
    R is R1 + T.

any_child(Pos, ListChild, R):- member(Pos, ListChild), R is 1.
any_child(Pos, ListChild, R):- not(member(Pos, ListChild)), R is 0.

dirtied( Count, List, ListDirty) :-
    Count > 0,
    length(List,A),
    not(A < Count),
    T is random(A),  
    nth0(List, T, Temp), 
    append([Temp], ListDirty, R),
    C is Count-1, 
    delete(List, Temp, R2), 
    dirtied(C , R2, R).

/*
my_select([], _,_,_,Result1,Result):- 
    Result is Result1.

my_select([X|List],ListCorral,ListDirty,ListObst , Result1, Result):- 
    validate_pos_dirt(X,ListCorral, ListDirty, ListObst),!, 
    append([X], Result1, R), 
    my_select(List, ListCorral, ListDirty, ListObst, R, Result) .
*/

my_sum([X,Y], [PosX,PosY], Result):- 
    A is (X + PosX), 
    B is (Y + PosY), 
    append([A],[B], Result).

my_append([X|Y], Result1, Result) :- 
    validate_pos([X|Y], N, M) , 
    append([X|Y], Result1, Result).

my_map([X],Pos, Result):-
    my_sum(X, Pos, R1),
    append([R1],[],Result).

my_map([X|List], Pos,  Result) :- 
    my_map(List, Pos, R1),
    my_sum(X, Pos, Temp),
    append([Temp],R1, Result).
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

sample(L, R) :- length(L, Len), random(0, Len, Random), nth0(Random, L, R).


my_dirt(Count2, R):- 
    Count2 > 2,!,
    R is 6.
my_dirt(Count2, R):-
    R is Count2.

child_dirty(Pos, ListChild, ListDirty, ListCorral, ListObst, P) :- 
    child_area_count(Pos, ListChild, Count1),
    Count2 is Count1 + 1, 
    %T is [[0,1],[1,0],[-1,0],[0,-1],[1,1],[1,-1],[-1,1],[-1,-1]] ,
    append([] ,[[0,1],[1,0],[-1,0],[0,-1],[1,1],[1,-1],[-1,1],[-1,-1]], T),
    my_map(T, Pos, Result ),
    my_dirt(Count2,Count),
    child_dirty_2(Result, Count, ListDirty, ListCorral, ListObst, P).


child_dirty_2(Result, 1,ListDirty,ListCorral,ListObst,P):-
    sample(Result, R) ,
    validate_pos_dirt(R,ListCorral,ListDirty,ListObst, P).

child_dirty_2(Result, Count , ListDirty, ListCorral, ListObst , P):-
    Temp is Count-1,
    Temp > 0,
    child_dirty_2(Result, Temp, ListDirty,ListCorral,ListObst, T),
    sample(Result, R),
    validate_pos_dirt(R, ListCorral,T,ListObst,P).

validate_pos_dirt(Pos,ListCorral,ListDirty,ListObst, P):- 
    not(member(Pos,ListCorral)) , 
    not(member(Pos,ListDirty)), 
    not(member(Pos, ListObst)),!,
    append(ListDirty, [Pos], P).
validate_pos_dirt(Pos,_,ListDirty,_, P):-
     append(ListDirty,[],P).

validate_move(Pos, R, ListObst, ListChild, ListCorral, ListRobot,ListDirty, NewChild, NewObst):-
    n(N),
    m(M),
    not(validate_pos(R,N,M)),
    append([],ListChild,NewChild),
    append([],ListObst,NewObst).
validate_move(Pos, R, ListObst, ListChild, ListCorral, ListRobot,ListDirty, NewChild, NewObst):-
    not(member(R,ListObst)),
    not(member(R,ListChild)),
    not(member(R,ListCorral)),
    not(member(R,ListRobot)),!,
    append([],ListObst, NewObst),
    my_update(ListChild, Pos, R, NewChild).
validate_move([X|Y], [RX|RY], ListObst, ListChild, ListCorral, ListRobot,ListDirty, NewChild, NewObst):-
    member(R,ListObst),!,
    DirX is RX - X,
    DirY is RY - Y,
    append([DirX],[DirY],Dir),
    push_obst([X|Y], Dir ,ListObst, ListChild, ListCorral, ListRobot,ListDirty, [RX|RY], _, NewObst),
    my_update(ListChild, [X|Y], [RX|RY], NewChild).

validate_move(Pos, R, ListObst, ListChild, ListCorral, ListRobot,ListDirty, NewChild, NewObst):-
    append(ListChild,[],NewChild),
    append(ListObst,[],NewObst).

push_obst(Pos, Dir, ListObst, ListChild, ListCorral, ListRobot,ListDirty,Original, NewChild, NewObst):- 
    %NewPos is Pos + Dir,
    my_sum(Pos,Dir,NewPos),
    member(NewPos,ListObst),
    push_obst(NewPos, Dir, ListObst, ListChild, ListCorral, ListRobot,ListDirty,Original, NewChild, NewObst).
push_obst(Pos, Dir, ListObst, ListChild, ListCorral, ListRobot,ListDirty,Original, _, NewObst):-
    %NewPos is Pos + Dir,
    my_sum(Pos,Dir,NewPos),
    not(member(NewPos,ListChild)),
    not(member(NewPos,ListCorral)),
    not(member(NewPos,ListRobot)),
    not(member(NewPos,ListObst)),
    not(member(NewPos,ListDirty)),
    my_update(ListObst, Original, NewPos, NewObst).    
push_obst(Pos, Dir, ListObst, ListChild, ListCorral, ListRobot,ListDirty,Original, NewChild, NewObst):-
    append(ListChild,[],NewChild),
    append(ListObst,[],NewObst).

child_move(Pos, ListDirty, ListObst, ListChild, ListCorral,ListRobot , NewChild, NewObst):-
    append([] ,[[0,1],[1,0],[-1,0],[0,-1],[1,1],[1,-1],[-1,1],[-1,-1]], T),
    my_map(T, Pos ,Result),
    sample(Result, R),
    validate_move(Pos, R, ListObst, ListChild, ListCorral, ListRobot,ListDirty, NewChild, NewObst).








child_agent(Pos, ListDirty, ListObst, ListChild, ListCorral,ListRobot, NewDirty,NewChild, NewObst) :- 
    child_dirty(Pos ,ListChild, ListDirty, ListCorral, ListObst, NewDirty) ,
   /* append(ListChild, [], NewChild),
    append(ListObst, [], NewObst). */
    child_move(Pos, NewDirty, ListObst, ListChild, ListCorral,ListRobot , NewChild, NewObst).
/*
start:-


    
    assert(n(10)),
    assert(m(10)),
    %push_obst(Pos, Dir, ListObst, ListChild, ListCorral, ListRobot,ListDirty,Original, NewChild, NewObst):-
    append([],[[1,2],[0,2],[1,3]],Child),
    append([],[[1,1],[2,2],[3,3]],Obst),
    print_general(10,10,[],Obst,Child,[[3,4]],[[4,4]]),
    %push_obst([0,0],[1,1], Obst,Child,[[4,4]],[],[],[1,1], NewChild, NewObst),
    child_agent([1,2],[],Obst,Child,[[4,4]],[[3,4]],NewDirty,NewChild,NewObst),
    nl,nl,nl,nl,
    print_general(10,10,NewDirty,NewObst,NewChild,[[3,4]],[[4,4]]).
    
    %my_update([[1,2],[2,3],[3,3],[1,1]],[1,1],[2,2],R),
   % write(R).
   % child_move()

start.
*/