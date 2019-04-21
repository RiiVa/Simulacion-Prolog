:-dynamic n/1, m/1 .
:- include(child).
:- include(childrens).
:- include(goal).
%:- include(paint).



    %el simulador 
    simulate(N,M,T,Child,Dirty,Obst):-
        setNM(N,M),
        simulate_2(N,M,T,Child,Dirty,Obst,0,ListObst,ListCorral,ListDirty,ListChild,ListBot,0,0,0) .

    
    
    simulate_2(N,M,T,Child,Dirty,Obst, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Fail):- 
        NT is T * 2,
        NT =:= Total,
        write("Robot Win =>"),write(Win),nl,
        write("Robot Lose =>"),write(Fail).

    simulate_2(N,M,T,Child,Dirty,Obst, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Fail):- 
        Temp is  Count mod T,
        Temp =:= 0,!,
        % (Total =:= 0; 
        %     (
        %         Fail1 
        %     )),
        generar_tablero(N,M,Child, Obst, Dirty, ListObst1,ListCorral1,ListDirty1,ListChild1,ListBot1),
        
        %print_general(N,M,ListDirty, ListObj, ListChild, ListBot, ListCorral),
        stupid_robot_agent(ListBot1, ListObst1, ListChild1, ListCorral1, ListDirty1, NewBot1, NewChild1, NewDirty1),
        % move_robot_to_child(NewBot, ListBot1, ListObst1, ListChild1),
        simulate_childs(N,M,ListObst1,ListCorral1,NewDirty1,NewChild1,NewChild1,NewBot1, _,NewDirty,NewObst,NewChild),
        Count1 is Temp + 1,
        print_general(N,M,NewDirty, NewObst, NewChild, NewBot1, ListCorral1),
        Total1 is Total + 1,
        simulate_2(N,M,T,Child,Dirty,Obst, Count1, NewObst,ListCorral1,NewDirty,NewChild,NewBot1,Total1,Win,Fail).
    simulate_2(N,M,T,Child,Dirty,Obst, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Fail):-
        win(ListChild,ListCorral,ListDirty),!,
        Count1 is Win +1,
        simulate_2(N,M,T,Child,Dirty,Obst, 0, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Count1,Fail).
    simulate_2(N,M,T,Child,Dirty,Obst, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Fail):-
        
        fail_test(ListDirty),!,
        Count1 is Fail +1,
        simulate_2(N,M,T,Child,Dirty,Obst, 0, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Count1).
    simulate_2(N,M,T,Child,Dirty,Obst, Count, ListObst,ListCorral,ListDirty,ListChild,ListBot,Total,Win,Fail):-
        
        stupid_robot_agent(ListBot, ListObst, ListChild, ListCorral, ListDirty, NewBot1, NewChild1, NewDirty1),
        % move_robot_to_child(NewBot, ListBot, ListObst, ListChild),
        simulate_childs(N,M,ListObst,ListCorral,NewDirty1,NewChild1,NewChild1,NewBot1, _,NewDirty,NewObst,NewChild),
        print_general(N,M,NewDirty, NewObst, NewChild, NewBot1, ListCorral),
        Count1 is Count + 1,
        Total1 is Total + 1,
        simulate_2(N,M,T,Child,Dirty,Obst, Count1, NewObst,ListCorral,NewDirty,NewChild,NewBot1,Total1,Win,Fail).

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[],ListBot, Count,NewDirty,NewObst,NewChild):- 
        Count is 0,
        append([],[],NewChild),
        append([],ListObst,NewObst),
        append([],ListDirty,NewDirty).

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[X],ListBot, Count,NewDirty,NewObst,NewChild):-
        simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[],ListBot, Temp,NewDirty1,NewObst1,NewChild1),
        Count is Temp + 1,
        child_agent(X,NewDirty1,NewObst1,ListChild,ListCorral, ListBot, NewDirty,NewChild,NewObst).

    simulate_childs(N,M,ListObst,ListCorral,ListDirty,ListChild,[X|Y],ListBot, Count,NewDirty,NewObst,NewChild):-
        simulate_childs(N, M, ListObst, ListCorral, ListDirty, ListChild,Y,ListBot, Temp,NewDirty1,NewObst1,NewChild1),
        Count is Temp + 1,
        
        child_agent(X,NewDirty1,NewObst1,NewChild1,ListCorral, ListBot, NewDirty,NewChild,NewObst).

    robot_ninja_agent(Pos,N,M, ListObst,ListCorral,ListDirty,ListChild, NewBot,NewDirty,NewChild):-
        member(Pos, ListDirty),
        delete(ListDirty, Pos, NewDirty),
        
        append([],ListChild,NewChild),
        append([Pos],[],NewBot).
    robot_ninja_agent(Pos,N,M, ListObst,ListCorral,ListDirty,ListChild, NewBot,NewDirty,NewChild):-
        
        pos_validate(Pos,N,M,ListObst, NewPos),
        take_child(Pos,NewPos,ListChild, NewChild),
        append([NewPos],[],NewBot),
        append([],ListDirty,NewDirty).

    take_child(_,NewPos,ListChild, NewChild):-
        not(member(NewPos,ListChild)),!,
        append([],ListChild, NewChild).
    take_child(Pos,NewPos,ListChild, NewChild):-
        my_update(ListChild,Pos,NewPos,NewChild).
    
    
    pos_validate(Pos,N,M,ListObst, NewPos):-
        append([] ,[[0,1],[1,0],[-1,0],[0,-1],[1,1],[1,-1],[-1,1],[-1,-1]], T),
        my_map(T,Pos,T1),
        sample(T1,NewPos),
        not(member(NewPos, ListObst)),
        validate_pos(NewPos,N,M),!.
    pos_validate(Pos,ListObst, NewPos):-
        pos_validate(Pos,ListObst, NewPos).
start:-
    simulate(3,3,50,15,15,15).
%     generar_tablero(6,6,9, 5, 5, ListObst1,ListCorral1,ListDirty1,ListChild1,[X|Y]),
%    % print_general(6,6,ListDirty1, ListObst1, ListChild1, [X|Y], ListCorral1),
%     robot_ninja_agent(X,6,6, ListObst1,ListCorral1,ListDirty1,ListChild1, [X1],NewDirty,NewChild),
%     print_general(6,6,NewDirty, ListObst1, NewChild, [X1], ListCorral1),
%     robot_ninja_agent(X1,6,6, ListObst1,ListCorral1,NewDirty,NewChild, NewBot,NewDirty1,NewChild1),
%     print_general(6,6,NewDirty1, ListObst1, NewChild1, NewBot, ListCorral1).
start.