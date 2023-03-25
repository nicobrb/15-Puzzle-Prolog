[library(dcg/basics)].

left.
rigth.
up.
down.

applicabile(up, BlankPos):-
	board(_,N), BlankPos-N>=0.
applicabile(down, BlankPos):-
	board(_,N), BlankPos+N < N*N.
applicabile(left, BlankPos):-
    board(_,N), (BlankPos mod N) > 0. 
applicabile(right, BlankPos):-
	board(_,N), BlankPos > 0, (BlankPos mod (N-1)) > 0.
applicabile(right, 0).


trasforma(PrevBoard, left, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N),
    LeftIdx is N - BlankPos,
    LeftDiff is (16<<(8*LeftIdx)) - (PrevBoard /\ (15<<(8*LeftIdx))),
    NextBoard is PrevBoard + (LeftDiff) - (LeftDiff>>8),
    NewBlankPos is BlankPos - 1.

trasforma(PrevBoard, right, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N),
    RightIdx is N - BlankPos - 2,
    RightDiff is (16<<(8*RightIdx)) - (PrevBoard /\ (15<<(8*RightIdx))),
    NextBoard is PrevBoard + (RightDiff) - (RightDiff<<8),
    NewBlankPos is BlankPos + 1.

trasforma(PrevBoard, up, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N),
    UpIdx is N - BlankPos + 3,
    UpDiff is (16<<(8*UpIdx)) - (PrevBoard /\ (15<<(8*UpIdx))),
    NextBoard is PrevBoard + (UpDiff) - (UpDiff>>(8*4)),
    NewBlankPos is BlankPos - 4.

trasforma(PrevBoard, down, BlankPos, NextBoard, NewBlankPos) :- 
    board(_, N),
    DownIdx is N - BlankPos - 5,
    DownDiff is (16<<(8*DownIdx)) - (PrevBoard /\ (15<<(8*DownIdx))),
    NextBoard is PrevBoard + (DownDiff) - (DownDiff<<(8*4)),
    NewBlankPos is BlankPos + 4.

fromHexToInteger(H, N) :-
    atom_concat('0x', H, HexaAtom),
    atom_codes(HexaAtom, HexaCodes),
    number_codes(N, HexaCodes).

fromIntegerToHex(N, H):- 
    phrase(xinteger(N), HexaCodes),
    atom_codes(H, HexaCodes). 

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

inverse(left, right).
inverse(right, left).
inverse(up, down).
inverse(down, up).

popAndAppend(List, Value, Result) :-
    select(First, List, Rest),  % select the first element and the rest of the list
    append(Rest, [Value], Result). % append the rest of the list and the first element


notMember(_, []) :- !.

notMember(X, [Head|Tail]) :-
        X \= Head,
    notMember(X, Tail).


% trasforma(up, S0, S1):-
% 	posV(V), board(_,N), NewPos is V-N, 
% 	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
% 	retractall(board(_,_)),assert(board(S1,N)).
% trasforma(down, S0, S1):-
% 	posV(V), board(_,N), NewPos is V+N, 
% 	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
% 	retractall(board(_,_)),assert(board(S1,N)).
% trasforma(left, S0, S1):-
% 	posV(V), board(_,N), NewPos is V-1, 
% 	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
% 	retractall(board(_,_)),assert(board(S1,N)).
% trasforma(right, S0, S1):-
% 	posV(V), board(_,N), NewPos is V+1, 
% 	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
% 	retractall(board(_,_)),assert(board(S1,N)).

% % To test this command use:
% % board(S0,_),trasforma(left,S0,S1),write(S0),write(S1).
% swap(V,NewPos,S0,S1):-
% 	% Create a list of V blank elements
% 	length(L1, V),
% 	% Create a list of NewPos blank elements
% 	length(L2, NewPos),
% 	% In L1 put the list 
% 	% (there will be all the elements before the one to swap)
% 	% Head1 contains the element to swap
% 	% Tail1 contains the remains elements of the list
% 	append(L1, [Head1|Tail1], S0),
% 	% Put in Temp the first and second part of the list
% 	% between the two parts put a variable
% 	append(L1, [Head2|Tail1], Temp),
% 	% Put in L2 the first part of the list without
% 	% the element to replace
% 	% Head2 contains the elements who will be replaced
% 	% Tail2 contains the second part of the list
% 	append(L2, [Head2|Tail2], Temp),
% 	append(L2, [Head1|Tail2], S1).

