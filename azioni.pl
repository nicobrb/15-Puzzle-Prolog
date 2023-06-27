inverse(left, right).
inverse(right, left).
inverse(up, down).
inverse(down, up).

applicabile(left, BlankPos):-
    board(_,N), (BlankPos mod N) > 0. 

applicabile(right, BlankPos):-
    board(_,N), 
    Q is div(BlankPos, N)+1,
    BlankPos =\= (Q*N - 1).

applicabile(up, BlankPos):-
    board(_,N), BlankPos-N>=0.

applicabile(down, BlankPos):-
    board(_,N), BlankPos+N < N*N.



trasforma(PrevBoard, left, BlankPos, NextBoard, NewBlankPos, SwappedValue) :- 
    board(_, N), Length is N*N,
    LeftIdx is  Length - BlankPos,
    ValueToSwap is (PrevBoard /\ (15<<(8*LeftIdx))),
    SwappedValue is (ValueToSwap>>(8*LeftIdx)),
    LeftDiff is (Length<<(8*LeftIdx)) - ValueToSwap,
    NextBoard is PrevBoard + (LeftDiff) - (LeftDiff>>8),
    NewBlankPos is BlankPos - 1.

trasforma(PrevBoard, right, BlankPos, NextBoard, NewBlankPos, SwappedValue) :- 
    board(_, N), Length is N*N,
    RightIdx is Length - BlankPos - 2,
    ValueToSwap is (PrevBoard /\ (15<<(8*RightIdx))),
    SwappedValue is (ValueToSwap>>(8*RightIdx)),
    RightDiff is (Length<<(8*RightIdx)) - ValueToSwap,
    NextBoard is PrevBoard + (RightDiff) - (RightDiff<<8),
    NewBlankPos is BlankPos + 1.

trasforma(PrevBoard, up, BlankPos, NextBoard, NewBlankPos, SwappedValue) :- 
    board(_, N), Length is N*N,
    UpIdx is Length - BlankPos + (N - 1),
    ValueToSwap is (PrevBoard /\ (15<<(8*UpIdx))),
    SwappedValue is (ValueToSwap>>(8*UpIdx)),
    UpDiff is (Length<<(8*UpIdx)) - ValueToSwap,
    NextBoard is PrevBoard + (UpDiff) - (UpDiff>>(8*N)),
    NewBlankPos is BlankPos - N.

trasforma(PrevBoard, down, BlankPos, NextBoard, NewBlankPos, SwappedValue) :- 
    board(_, N), Length is N*N,
    DownIdx is Length - BlankPos - (N + 1),
    ValueToSwap is (PrevBoard /\ (15<<(8*DownIdx))),
    SwappedValue is (ValueToSwap>>(8*DownIdx)),
    DownDiff is (Length<<(8*DownIdx)) - ValueToSwap,
    NextBoard is PrevBoard + (DownDiff) - (DownDiff<<(8*N)),
    NewBlankPos is BlankPos + N.


