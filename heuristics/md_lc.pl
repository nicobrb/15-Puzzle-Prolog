% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, ?Res)
% CurrPos is the current board in hex format
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
    Val is CellVal - 1,
    divmod(Val, N, X1, Y1),
    NextIdx is CellIdx - 1,
    divmod(NextIdx, N, X2, Y2),
    PartialPos is CurrPos >> 8,
    manhattan(PartialPos, NextIdx, N, Dim, PartialSum),
    rowconflict(CurrPos,CellVal,CellIdx,X1,X2,Y2,RConflict),
    columnconflict(CurrPos,CellVal,CellIdx,Y1,Y2,X2,CConflict),
    Sum is abs(X1-X2) + abs(Y1-Y2) + PartialSum+ RConflict+CConflict.


manhattan(CurrPos, CellIdx, N, Dim, Sum) :-
    NextIdx is CellIdx - 1,
    PartialPos is CurrPos >> 8,
    manhattan(PartialPos, NextIdx, N, Dim, Sum).

rowconflict(0,_,0,_,_,_,0).
rowconflict(CurrPos,CellVal,CellIdx,X1,X2,Check,RConflict):-
    Check>=0,
    X1==X2,
    board(_,N),
    Dim is N*N,
    PartialPos is CurrPos >> 8,
    CellValS is (PartialPos mod 16),
    CellValS =\= Dim mod 16, % this mod is needed to generalize to n-puzzle
    Val is CellValS - 1,    
    divmod(Val, N, X3, Y3),
    NextIdx is CellIdx - 1,
    divmod(NextIdx, N, X4, Y4),
    %X4==Check,
    X3==X4,
    CellVal<CellValS,
    NewCheck is Check-1,
    rowconflict(PartialPos,CellVal,NextIdx,X1,X2,NewCheck,NewRConflict),
    RConflict is NewRConflict+2.

rowconflict(CurrPos,CellVal,CellIdx,X1,X2,Check,RConflict):-
    Check>=0,
    X1==X2,
    NewCheck is Check-1,
    PartialPos is CurrPos >> 8,
    NextIdx is CellIdx - 1,
    rowconflict(PartialPos,CellVal,NextIdx,X1,X2,NewCheck,RConflict).

rowconflict(_,_,_,_,_,_,0).
columnconflict(0,_,0,_,_,_,0).
columnconflict(CurrPos,CellVal,CellIdx,Y1,Y2,Check,CConflict):-
    Check>=0,
    Y1==Y2,
    board(_,N),
    Dim is N*N,
    PartialPos is CurrPos >> (8*N),
    CellValS is (PartialPos mod 16),
    CellValS =\= Dim mod 16, % this mod is needed to generalize to n-puzzle
    Val is CellValS - 1,
    divmod(Val, N, X3, Y3),
    NextIdx is CellIdx - 1,
    divmod(NextIdx, N, X4, Y4),
    %X4==Check,
    Y3==Y4,
    CellVal<CellValS,
    NewCheck is Check-N,
    columnconflict(PartialPos,CellVal,NextIdx,Y1,Y2,NewCheck,NewCConflict),
    CConflict is NewCConflict+2.

columnconflict(CurrPos,CellVal,CellIdx,Y1,Y2,Check,CConflict):-
    Check>=0,
    Y1==Y2,
    NewCheck is Check-1,
    board(_,N),
    PartialPos is CurrPos >> 8*N,
    NextIdx is CellIdx - N,
    columnconflict(PartialPos,CellVal,NextIdx,Y1,Y2,NewCheck,CConflict).
  
columnconflict(_,_,_,_,_,_,0).