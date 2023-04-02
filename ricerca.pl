:-['dominio.pl'].
:-['azioni.pl'].
:-['heuristics.pl'].

initialize:- retractall(depth(_)),
            assert(depth(1)).
:-initialize.

prova(Soluzione) :- 
    board(List,N), 
    solvable(List),
    depth(Depth),
    nth0(BlankPos,List,v),
    Max is N*N,
    replace(List,BlankPos,Max,NewList),
    hex_bytes(Hex,NewList),
    fromHexToInteger(Hex,StartingBoard),
    iterativeDeepening(StartingBoard, BlankPos, Depth, Soluzione),
    write(Soluzione).

prova(Soluzione):-
    depth(Depth),
    NewDepth is Depth+1,
    retractall(depth(_)),
    assert(depth(NewDepth)),
    prova(Soluzione).

iterativeDeepening(StartingBoard,BlankPos, Depth, Solution):-
    write('---- Depth: '), write(Depth),write('\n'),
    nextMove(StartingBoard,Solution,BlankPos, V, Depth).


nextMove(Position, [], BlankPos, LastMove, MaxDepth):-
    goal(Solution), 
    Position == Solution,
    !,
    depth(Depth),
    write('Solution found at depth '), write(Depth),write('!\n').


nextMove(Position,[Move|MoveList],BlankPos,LastMove,MaxDepth):-
    MaxDepth > 0,
    inverse(Move,Inverse),
    Inverse \== LastMove,
    applicabile(Move,BlankPos), 
    %notMember(Move, [LastMove|PreviousTwoMoves]),
    trasforma(Position,Move,BlankPos,NewPosition,NewBlankPos),
    % popAndAppend([LastMove|PreviousTwoMoves],Inverse,LastMoves),
    NewDepth is MaxDepth-1,
    nextMove(NewPosition,MoveList,NewBlankPos,Move,NewDepth).


