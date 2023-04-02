% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, ?Res)
% CurrPos is the current board in hex format
heuristic(CurrPos, Res) :-
    manhattan(CurrPos, Res).

manhattan(CurrPos, Res) :-
    board(_, N), Max is N*N,
    manhattan(CurrPos, Max, N, Max, Res), nodebug.

manhattan(_, 0, _, _, 0).

manhattan(CurrPos, CellIdx, N, Dim, Sum) :-
    CellVal is (CurrPos mod 16),
    CellVal =\= Dim mod 16, % this mod is needed to generalize to n-puzzle
    Val is CellVal - 1,
    divmod(Val, N, X1, Y1),
    NextIdx is CellIdx - 1,
    divmod(NextIdx, N, X2, Y2),
    PartialPos is CurrPos >> 8,
    manhattan(PartialPos, NextIdx, N, Dim, PartialSum),
    Sum is abs(X1-X2) + abs(Y1-Y2) + PartialSum.

manhattan(CurrPos, CellIdx, N, Dim, Sum) :-
    NextIdx is CellIdx - 1,
    PartialPos is CurrPos >> 8,
    manhattan(PartialPos, NextIdx, N, Dim, Sum).
