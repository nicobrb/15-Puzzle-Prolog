:-['manhattan.pl'].
% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, ?Res)
% CurrPos is the current board in hex format
heuristic(CurrPos, Res) :-
    manhattan(CurrPos, M),
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, Lc, N, 1,NewN),
    Res is M+(2*Lc),
    !.

% Stop is used to define the case base
% When I reach N+1 the linear rule needs to stop.
linear(_, Temp,Lc,_,Stop,Stop):-Lc is Temp.
linear(CurrPos, TempLc,Lc, N, I,Stop):-
    I=<(N+1),
    % Obtain the I-th row
    Partial is CurrPos >> (8 * N * I),
    PartialSecond is Partial << (8 * N * I),
    Row is CurrPos - PartialSecond,
    Idx is N - 1,
    J is N-I,
    %Check conflicts of the row
    conflict(Row, J, Idx, R, "R",0, -1,Idx),
    NewIdx is I-1,
    %Find the NewIdx-th (I-1) column
    findColumn(CurrPos,-1,0,Col,_,NewIdx),
    %Check conflicts of the column
    conflict(Col,J, Idx, C, "C", 0, -1,Idx),
    NewI is I+1,
    NewTempLc is TempLc + C + R,
    linear(CurrPos, NewTempLc, Lc, N, NewI,Stop),!.

% Base case that occur when I is -1
conflict(_,_,(-1),Result,_,Temp,_,_):-Result is Temp,!.
% Hex of the row/column to check 
% J Jolly the row/column pos 
% I index to check all values of the hex
% Type (can be C or R) used to know if the check is about a row or a column
% Val value to check with the others
% IdxVal index of the value
% This rule is activated when its needed the value to check
conflict(Hex, J, I, Result, Type, Temp, Val, IdxVal):-
    Val = -1,
    I >= 0,
    NewHex is Hex >> (8 * I),
    NewVal is (NewHex mod 16),
    NewVal =\= 0,
    CurrVal is NewVal - 1,
    board(_,N),
    divmod(CurrVal,N,X1,Y1),
    isTarget(X1,Y1,J,Type),
    NewI is I-1,
    conflict(Hex, J,NewI, Result, Type, Temp, NewVal,IdxVal).

% If Val !=-1 (so there is a value to check)
% Val is compared to the rest of the elements in the hex
conflict(Hex, J,I, Result, Type, Temp, Val,IdxVal):-
    Val =\= -1,
    I >= 0,
    NewHex is Hex >> (8 * I),
    NewVal is (NewHex mod 16),
    board(_,N),
    NewVal =\= ((N*N) mod 16),
    CurrVal is NewVal - 1,
    %POS DEL VALORE
    board(_,N),
    divmod(CurrVal,N,X1,Y1),
    isTarget(X1,Y1,J,Type),
    Val>NewVal,
    NewTemp is Temp+1,
    NewI is I-1,
    conflict(Hex,J,NewI,Result,Type,NewTemp,Val,IdxVal).
% If the Val or value to compare is not in 
% their destination row/column updates index
conflict(Hex,J,I,Result,Type, Temp, Val, IdxVal):-
    %write("ENTRA3"),
    I>=0,
    NewI is I-1,
    conflict(Hex,J,NewI,Result,Type,Temp,Val, IdxVal).
% It occurs when I is not >=0 so the Hex has been
% all checked and this is the first assignment to Temp
conflict(Hex,J,I,Result,Type,Temp,_,IdxVal):-
    I<0,
    I>(-1),
    board(_,N),
    NewN is N-1,
    IdxVal=NewN,
    NewIdxVal is IdxVal-1,
    Result is Temp,
    conflict(Hex,J,(N-2),Result,Type,0,-1,NewIdxVal). 
% The same as the previous but in this case this is not
% the first assignment to Temp (Temp is not null)       
conflict(Hex,J,I,Result,Type,Temp,Val,IdxVal):-
    %write("ENTRA5"),
    I<0,
    I>(-3),
    Val=\=(-1),
    Temp > Result,
    NewC is Temp,
    NewIdxVal is IdxVal-1,
    NewI is NewIdxVal-1,
    conflict(Hex,J,NewI,NewC,Type,0,-1,NewIdxVal).

%Rules to check if a value is in its row/column target
isTarget(X1,_,J,Type):-
    Type == "R",
    X1 == J,!.
isTarget(_,Y1,J,Type):-
    Type == "C",
    Y1 == J.

%Rules to obtain the column to check
% Base rule when I is equal to N
findColumn(_,N,N,Col,Temp,_):- Col is Temp,!.
%First rule to occur, init NewTemp, it occurs once for every column.
findColumn(CurrPos,-1,I,Col,_,Idx):-
    board(_,N),
    NewTemp is 0,
    findColumn(CurrPos,N,I,Col,NewTemp,Idx).
%This two rules are the same
% the only differece is that the first if met 0 means
% that found the empty space so the rule put in the hex
% the maximum number of the N*N puzzle 
% Example: 4x4 board, the empty space value is 16.
findColumn(CurrPos,N,I,Col,Temp,Idx):-
    I<N,
    El is ((CurrPos >> ((8 * N * I) + (8*Idx))) mod 16),
    El = 0,
    NewTemp is Temp + ((N*N) << (8*I)),
    NewI is I+1,
    findColumn(CurrPos,N,NewI,Col,NewTemp,Idx).
findColumn(CurrPos,N,I,Col,Temp,Idx):-
    I<N,
    El is ((CurrPos >> ((8 * N * I) + (8*Idx))) mod 16),
    NewTemp is Temp + (El << (8*I)),
    NewI is I+1,
    findColumn(CurrPos,N,NewI,Col,NewTemp,Idx).
    