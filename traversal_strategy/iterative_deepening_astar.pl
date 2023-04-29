:-['heuristics/manhattan.pl'].

initialize:- 
    retractall(h(_)),
    board(BoardList, N),
    nth0(BlankPos, BoardList, v),
    Max is N*N,
    replace(BoardList, BlankPos, Max, NewList),
    hex_bytes(Hex, NewList),
    fromHexToInteger(Hex, StartingBoard),
    heuristic(StartingBoard, StartingCost),
    assert(h(StartingCost)),
    assert(original(StartingCost)).
        
:-initialize.

ida(StartingBoard, BlankPos, Solution):-
    h(Cost),
    write('---- Starting Depth Bound: '), write(Cost), write('\n'),
    idastar(StartingBoard, Solution, Cost, BlankPos).
    
idastar(StartingBoard, Solution, Cost, BlankPos):-
    original(OriginalCost),
    originalR(R),
    originalC(C),
    nextMoveWithHeuristics(StartingBoard, Solution, Cost, OriginalCost, R, C,BlankPos, _, 1).

idastar(StartingBoard, Solution, CostToUpdate, BlankPos):-
    findall(NodeCost, possibleNode(_, NodeCost), NodeCostList),
    exclude(>=(CostToUpdate), NodeCostList, ExceedingCostList),
    sort(ExceedingCostList, SortedList),
    nth0(0, SortedList, NewCost),
    retractall(possibleNode(_,_)),
    retract(h(_)),
    assert(h(NewCost)),
    ida(StartingBoard, BlankPos, Solution).

nextMoveWithHeuristics(Position, [], _, _, _, _, _, _, G):-
    goal(Solution), 
    Position == Solution,
    !,
    NewG is G-1,
    write('Solution found at depth '), write(NewG), write('!\n').

nextMoveWithHeuristics(Position, [Move|MoveList], BoundCost, LastCost, LastLCRow, LastLCCol, BlankPos, LastMove, G):-
    inverse(Move, Inverse),
    Inverse \== LastMove,
    applicabile(Move, BlankPos), 
    trasforma(Position, Move, BlankPos, NewPosition, NewBlankPos, SwappedValue),
    % heuristic(NewPosition, PositionCost),
    board(_, N),
    cellDistance(SwappedValue, NewBlankPos, N, OldCellManhattanDistance),
    cellDistance(SwappedValue, BlankPos, N, NewCellManhattanDistance),
    checkMove(Move,NewPosition,ResLC,LastLCRow,LastLCCol,NewLastLCRow,NewLastLCCol),
    NewManahattan is (LastCost - OldCellManhattanDistance + NewCellManhattanDistance),
    %writeln(LastLCRow+","+LastLCCol+","+NewLastLCRow+","+NewLastLCCol), 
    PositionCost is (NewManahattan+ResLC-LastLCRow-LastLCCol), 
    F is G + PositionCost,
    assert(possibleNode(NewPosition, F)),
    F =< BoundCost,
    NewG is G + 1,
    nextMoveWithHeuristics(NewPosition, MoveList, BoundCost, PositionCost, NewLastLCRow, NewLastLCCol, NewBlankPos, Move, NewG).
