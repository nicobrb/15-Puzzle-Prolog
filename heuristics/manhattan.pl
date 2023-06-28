% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, ?Res)
% CurrPos is the current board in hex format

cellDistance(CellVal, CellIdx, N, Distance):-
    Val is CellVal - 1,
    divmod(Val, N, X1, Y1),
    divmod(CellIdx, N, X2, Y2),
    Distance is integer(1.8*(abs(X1-X2) + abs(Y1-Y2))).

heuristic(CurrPos, Res) :-
    manhattan(CurrPos, Res),
    !.

manhattan(CurrPos, Res) :-
    board(_, N), Max is N*N,
    manhattan(CurrPos, Max, N, Max, Res).

manhattan(_, 0, _, _, 0).

manhattan(CurrPos, CellIdx, N, Dim, Sum) :-
    CellVal is (CurrPos mod 16),
    CellVal =\= Dim mod 16, % this mod is needed to generalize to n-puzzle
    NextIdx is CellIdx - 1,
    cellDistance(CellVal, NextIdx, N, Distance),
    PartialPos is CurrPos >> 8,
    
    manhattan(PartialPos, NextIdx, N, Dim, PartialSum),
    Sum is (Distance + PartialSum).

manhattan(CurrPos, CellIdx, N, Dim, Sum) :-
    NextIdx is CellIdx - 1,
    PartialPos is CurrPos >> 8,
    manhattan(PartialPos, NextIdx, N, Dim, Sum).

test(Res):- 
    board(BoardList, N),
    nth0(BlankPos, BoardList, v),
    Max is N*N,
    replace(BoardList, BlankPos, Max, NewList),
    hex_bytes(Hex, NewList),
    fromHexToInteger(Hex, StartingBoard),
    heuristic(StartingBoard, Res).
