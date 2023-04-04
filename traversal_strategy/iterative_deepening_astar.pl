:-['heuristics.pl'].

initialize:- retractall(h(_)),
            board(BoardList, N),
            nth0(BlankPos, BoardList, v),
            Max is N*N,
            replace(BoardList, BlankPos, Max, NewList),
            hex_bytes(Hex, NewList),
            fromHexToInteger(Hex, StartingBoard),
            heuristic(StartingBoard, StartingCost),
            assert(h(StartingCost)).
        
:-initialize.

ida(StartingBoard, BlankPos, Solution):-
    h(Cost),
    write('---- Starting Depth Bound: '), write(Cost), write('\n'),
    idastar(StartingBoard, Solution, Cost, BlankPos).

% iterativeDeepeningAStar(StartingBoard, BlankPos, Depth, Solution):-
%     % write('---- Depth: '), write(Depth),write('\n'),
%     heuristic(StartingBoard, StartingCost),
%     nextMoveWithHeuristics(StartingBoard, Solution, StartingCost, BlankPos, V, Depth).
    
idastar(StartingBoard, Solution, Cost, BlankPos):-
    nextMoveWithHeuristics(StartingBoard, Solution, Cost, BlankPos, Move, 1).
    % findall(NodeCost, possibleNode(_, NodeCost), NodeCostList),
    % exclude(>=(StartingCost), NodeCostList, ExceedingCostList),
    % sort(ExceedingCostList, SortedList),
    % nth0(0, SortedList, NewCost),
    % retractall(possibleNode(_,_)),
    % idastar(StartingBoard, Solution, NewCost, BlankPos).

idastar(StartingBoard, Solution, CostToUpdate, BlankPos):-
    findall(NodeCost, possibleNode(_, NodeCost), NodeCostList),
    exclude(>=(CostToUpdate), NodeCostList, ExceedingCostList),
    sort(ExceedingCostList, SortedList),
    nth0(0, SortedList, NewCost),
    retractall(possibleNode(_,_)),
    retract(h(_)),
    assert(h(NewCost)),
    ida(StartingBoard, BlankPos, Solution).
    %vidastar(StartingBoard, Solution, NewCost, BlankPos),
    % nextMoveWithHeuristics(StartingBoard, Solution, NewCost, BlankPos, Move, 1).

nextMoveWithHeuristics(Position, [], BoundCost, BlankPos, LastMove, G):-
    goal(Solution), 
    Position == Solution,
    !,
    NewG is G-1,
    write('Solution found at depth '), write(NewG), write('!\n').

nextMoveWithHeuristics(Position, [Move|MoveList], BoundCost, BlankPos, LastMove, G):-
    inverse(Move, Inverse),
    Inverse \== LastMove,
    applicabile(Move, BlankPos), 
    trasforma(Position, Move, BlankPos, NewPosition, NewBlankPos),
    % nodebug,
    heuristic(NewPosition, PositionCost),
    % trace,
    F is G + PositionCost,
    assert(possibleNode(NewPosition, F)),
	F =< BoundCost,
    NewG is G + 1,
    nextMoveWithHeuristics(NewPosition, MoveList, BoundCost, NewBlankPos, Move, NewG).

