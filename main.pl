generate_pos(BoardHeigth,BoardWidth,Dirty,Obstacles,Childs,Corral,Robots,Pos):-
	A is random(5),
	B is random(20),
	append([A],[B],Pos),
	not(member(Pos,Dirty)),
	not(member(Pos,Obstacles)),
	not(member(Pos,Childs)),
	not(member(Pos,Corral)),
	not(member(Pos,Robots)),!.

generate_pos(BoardHeigth,BoardWidth,Dirty,Obstacles,Childs,Corral,Robots,Pos):-
		generate_pos(BoardHeigth,BoardWidth,Dirty,Obstacles,Childs,Corral,Robots,Pos).

generate_env(BoardHeigth,BoardWidth,Agent,DirtinessCount,ObstacleCount,ChildsCount,Dirty,Obstacles,Childs,Corral,Robots,ResultObstacles):-
	ObstacleCount > 0,
	X is ObstacleCount-1,
	generate_pos(BoardHeigth,BoardWidth,Dirty,Obstacles,Childs,Corral,Robots,Pos),
	Result = [Pos|Obstacles], 
	generate_env(BoardHeigth,BoardWidth,Agent,DirtinessCount,X,ChildsCount,Dirty,Result,Childs,Corral,Robots,Result2),
	append(Result,Result2,ResultObstacles), !.
	%ResultObstacles = [Result|Result2].

generate_env(BoardHeigth,BoardWidth,Agent,DirtinessCount,0,ChildsCount,Dirty,Obstacles,Childs,Corral,Robots,ResultObstacles):- 
	ResultObstacles = [].
			


main:-
	Agent = 5,
	BoardHeigth = 5,
	BoardWidth = 5,
	Time = 0,
	TimeChange = 5,
	DirtinessPercent = 17,
	DirtinessCount is round((DirtinessPercent/100)*BoardHeigth*BoardWidth),
	ObstaclePercent = 10,
	ObstacleCount is round((ObstaclePercent/100)*BoardHeigth*BoardWidth),
	ChildsCount = 10,
	Dirty = [],
	Obstacles = [],
	Childs = [],
	Corral = [],
	Robots = [],
	generate_env(BoardHeigth,BoardWidth,Agent,DirtinessCount,ObstacleCount,ChildsCount,Dirty,Obstacles,Childs,Corral,Robots,ObstaclesEnv),
	write(ObstaclesEnv).
	%write(ObstaclesEnv).
	