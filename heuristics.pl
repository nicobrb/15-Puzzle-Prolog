% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, +EndPos, ?Res)
% CurrPos is the current board in hex format
heuristic(CurrPos, Res) :-
    manhattan(CurrPos, Res).

manhattan(CurrPos, Res) :-
    board(_, N), Max is N*N,
    manhattan(CurrPos, Max, N, Res).
manhattan(_, 0, _, 0).
manhattan(CurrPos, CellIdx, N, Sum) :-
    % A 1 is subtracted from CellIdx and CellVal to make the module work 
    % otherwise with the Max value the algorithm does not return the expected value
    NextIdx is CellIdx - 1,
    divmod(NextIdx, N, X2, Y2),
    % 256 is 2^8, i.e. as many as there are bits to 'shift'.
    divmod(CurrPos, 256, PartialPos, CellVal),
    Val is CellVal - 1,
    divmod(Val, N, X1, Y1),
    manhattan(PartialPos, NextIdx, N, PartialSum),
    Sum is abs(X1-X2) + abs(Y1-Y2) + PartialSum.
