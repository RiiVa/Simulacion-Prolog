insert_char(Pos, _, _, ListChild, ListBot, ListCorral, R):- 
    member(Pos, ListCorral), 
    member(Pos, ListBot),
    member(Pos, ListChild),!,
    string_concat('',' [RC]|', R).
insert_char(Pos, ListDirty, _, ListChild, ListBot, _, R):- 
    member(Pos, ListDirty), 
    member(Pos, ListBot),
    member(Pos, ListChild),!,
    string_concat('',' *RC*|', R).
insert_char(Pos, ListDirty, _, ListChild, _, _, R):- 
    member(Pos, ListDirty), 
    member(Pos, ListChild),!,
    string_concat('','  *C*|', R).
insert_char(Pos, ListDirty, _, _, ListBot, _, R):- 
    member(Pos, ListDirty), 
    member(Pos, ListBot),!,
    string_concat('','  *R*|', R).
insert_char(Pos, _, _, ListChild, _, ListCorral, R):- 
    member(Pos, ListCorral), 
    member(Pos, ListChild),!,
    string_concat('','  [C]|', R).
insert_char(Pos, _, _, _, ListBot, ListCorral, R):- 
    member(Pos, ListCorral), 
    member(Pos, ListBot),!,
    string_concat('','  [R] ', R).
insert_char(Pos, _, _, ListChild, ListBot, _, R):- 
    member(Pos, ListBot), 
    member(Pos, ListChild),!,
    string_concat('','  RC |', R).
insert_char(Pos, _, _, _, ListBot, _, R):- 
    member(Pos, ListBot),!,
    string_concat('','  R  |', R).
insert_char(Pos, _, _, ListChild, _, _, R):- 
    member(Pos, ListChild),!,
    string_concat('','  C  |', R).
insert_char(Pos, ListDirty, _, _, _, _, R):- 
    member(Pos, ListDirty),!,
    string_concat('','  ** |', R).
insert_char(Pos, _, ListObj, _, _, _, R):- 
    member(Pos, ListObj),!,
    string_concat('','  O  |', R).
insert_char(Pos, _, _, _, _, ListCorral, R):- 
    member(Pos, ListCorral),!,
    string_concat('','  [ ]|', R).
insert_char(_, _, _, _, _, _, R):-
    string_concat('','     |', R).

for_line(N, 0, ListDirty, ListObj, ListChild, ListBot, ListCorral, String , R):- 
    insert_char([N,0], ListDirty, ListObj, ListChild, ListBot, ListCorral, R1), 
    string_concat(String, R1, R).

for_line(N, M, ListDirty, ListObj, ListChild, ListBot, ListCorral, String , R):-
    Temp is M-1,
    Temp > -1,
    for_line(N, Temp, ListDirty, ListObj, ListChild, ListBot, ListCorral, String , R1),
    insert_char([N,M], ListDirty, ListObj, ListChild, ListBot, ListCorral, R2),
    string_concat(R1,R2,R ).

for_board(0, M , ListDirty, ListObj, ListChild, ListBot, ListCorral , R) :-
    print_line(M, '', RL),
    append([], [RL], RL2) ,
    string_concat('|','',Stick),
    for_line(0, M, ListDirty, ListObj, ListChild, ListBot, ListCorral, Stick , R1),
    append(RL2, [R1], R3),
    append(R3, [RL], R).

for_board(N, M , ListDirty, ListObj, ListChild, ListBot, ListCorral , R) :- 
    Temp is N-1,
    Temp > -1,
    for_board(Temp, M , ListDirty, ListObj, ListChild, ListBot, ListCorral , R1),
    string_concat('|','',Stick),
    for_line(N, M, ListDirty, ListObj, ListChild, ListBot, ListCorral, Stick , R2),
    append(R1, [R2], R3),
    print_line(M,'', RL),
    append(R3, [RL] ,R).


print_line(0, String,R) :- string_concat(String, '-------', R).
print_line(M, String, R):- Temp is M-1, print_line(Temp, String, R2), string_concat(R2,'------',R).

print_board([X]):- writeln(X).
print_board([X|Y]):- writeln(X),  print_board(Y).

print_general(N,M,ListDirty, ListObj, ListChild, ListBot, ListCorral):- 
    Temp1 is N-1,
    Temp2 is M-1,
    for_board(Temp1,Temp2,ListDirty, ListObj, ListChild, ListBot, ListCorral, R), !,
    print_board(R).