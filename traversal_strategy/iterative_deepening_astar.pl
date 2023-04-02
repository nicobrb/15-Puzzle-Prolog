:-['heuristics.pl'].

ida(StartingBoard, BlankPos, Solution):-
    heuristic(StartingBoard, StartingCost),
    write('---- Starting Depth Bound: '), write(StartingCost), write('\n'),
    idastar(StartingBoard, Solution, StartingCost, BlankPos).

% iterativeDeepeningAStar(StartingBoard, BlankPos, Depth, Solution):-
%     % write('---- Depth: '), write(Depth),write('\n'),
%     heuristic(StartingBoard, StartingCost),
%     nextMoveWithHeuristics(StartingBoard, Solution, StartingCost, BlankPos, V, Depth).
    
idastar(StartingBoard, Solution, StartingCost, BlankPos):-
    nextMoveWithHeuristics(StartingBoard, Solution, StartingCost, BlankPos, Move, 1),
    findall(NodeCost, possibleNode(_, NodeCost), NodeCostList),
    exclude(>=(StartingCost), NodeCostList, ExceedingCostList),
    sort(ExceedingCostList, SortedList),
    nth0(0, SortedList, NewCost),
    retractall(possibleNode(_,_)),
    idastar(StartingBoard, Solution, NewCost, BlankPos).

nextMoveWithHeuristics(Position, [], BoundCost, BlankPos, LastMove, G):-
    goal(Solution), 
    Position == Solution,
    !,
    write('Solution found at depth '), write(G), write('!\n').

nextMoveWithHeuristics(Position, [Move|MoveList], BoundCost, BlankPos, LastMove, G):-
    inverse(Move, Inverse),
    Inverse \== LastMove,
    applicabile(Move, BlankPos), 
    trasforma(Position, Move, BlankPos, NewPosition, NewBlankPos),
    heuristic(NewPosition, PositionCost),
    F = G + PositionCost,
    assert(possibleNode(NewPosition, PositionCost)),
	PositionCost =< BoundCost,
    NewG is G + 1,
    nextMoveWithHeuristics(NewPosition, MoveList, BoundCost, NewBlankPos, Move, NewG).

