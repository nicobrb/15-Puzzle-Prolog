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



trasforma(PrevBoard, left, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N), Length is N*N,
    LeftIdx is  Length - BlankPos,
    LeftDiff is (Length<<(8*LeftIdx)) - (PrevBoard /\ (15<<(8*LeftIdx))),
    NextBoard is PrevBoard + (LeftDiff) - (LeftDiff>>8),
    NewBlankPos is BlankPos - 1.

trasforma(PrevBoard, right, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N), Length is N*N,
    RightIdx is Length - BlankPos - 2,
    RightDiff is (Length<<(8*RightIdx)) - (PrevBoard /\ (15<<(8*RightIdx))),
    NextBoard is PrevBoard + (RightDiff) - (RightDiff<<8),
    NewBlankPos is BlankPos + 1.

trasforma(PrevBoard, up, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N), Length is N*N,
    UpIdx is Length - BlankPos + (N - 1),
    UpDiff is (Length<<(8*UpIdx)) - (PrevBoard /\ (15<<(8*UpIdx))),
    NextBoard is PrevBoard + (UpDiff) - (UpDiff>>(8*N)),
    NewBlankPos is BlankPos - N.

trasforma(PrevBoard, down, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N), Length is N*N,
    DownIdx is Length - BlankPos - (N + 1),
    DownDiff is (Length<<(8*DownIdx)) - (PrevBoard /\ (15<<(8*DownIdx))),
    NextBoard is PrevBoard + (DownDiff) - (DownDiff<<(8*N)),
    NewBlankPos is BlankPos + N.



