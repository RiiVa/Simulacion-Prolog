:-dynamic n/1, m/1.
:-include(child).
:-include(childrens).



    
    simulate(N,M,T,Child,Dirty,Obst):-
        NT is T * 100,
        simulate_2(N,M,T,Child,Dirty,Obst,0,ListObst,ListCorral,ListDirty,ListChild,ListBot) .


    simulate_2(N,M,T,Child,Dirty, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot):- 
        Temp is  Count mod T,
        Temp =:= 0,!,
        generar_tablero(N,M,Child, Obst, Dirty, ListObst,ListCorral,ListDirty,ListChild,ListBot),
        
        print_general(N,M,ListDirty, ListObj, ListChild, ListBot, ListCorral),
        
        simulate_2(N,M,T,Child,Dirty, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot),
        print_general(N,M,ListDirty, ListObj, ListChild, ListBot, ListCorral).

     simulate_2(N,M,T,Child,Dirty, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot):-
        simulate_childs(),
        
        simulate_2(N,M,T,Child,Dirty, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot).

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[],ListBot, Count):- Count.

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[X],ListBot, Count):-
        Temp is Count + 1,
        child_agent(X,ListDirty,ListObst,ListChild,ListCorral, ListBot, NewDirty, NewChild, NewObst),
        simulate_Childs(N,M,NewObst,ListCorral,NewDirty,NewChild,[],ListBot, Count).

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[X|Y],ListBot, Count):-
        Temp is Count + 1,
        
        child_agent(X,ListDirty,ListObst,ListChild,ListCorral, ListBot, NewDirty, NewChild, NewObst),
        simulate_childs(N, M, NewObst, ListCorral, NewDirty, NewChild,Y,ListBot, Temp).

start:-
    simulate(10,5,40,2,3,1),
    fail.
start.