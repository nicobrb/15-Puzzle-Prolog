initialize:- retractall(depth(_)),
            assert(depth(1)).
:-initialize.

iterativeDeepening(StartingBoard, BlankPos, Solution):-
    depth(Depth),
    write('---- Depth: '), write(Depth), write('\n'),
    nextMove(StartingBoard, Solution, BlankPos, _, Depth).

iterativeDeepening(StartingBoard, BlankPos, Solution):-
    depth(Depth),
    NewDepth is Depth+1,
    retractall(depth(_)),
    assert(depth(NewDepth)),
    iterativeDeepening(StartingBoard, BlankPos, Solution).


 nextMove(Position, [], _, _, _):-
     goal(Solution), 
     Position == Solution,
     !,
     depth(Depth),
     write('Solution found at depth '), write(Depth), write('!\n').


nextMove(Position,[Move|MoveList],BlankPos,LastMove,MaxDepth):-
    MaxDepth > 0,
    inverse(Move, Inverse),
    Inverse \== LastMove,
    applicabile(Move, BlankPos), 
    trasforma(Position, Move, BlankPos, NewPosition, NewBlankPos),
    NewDepth is MaxDepth-1,
    nextMove(NewPosition, MoveList, NewBlankPos, Move, NewDepth).
