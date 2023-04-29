:-['manhattan.pl'].
% predicate wrapper to allow the heuristics to be changed transparently in the search method
% heuristic(+CurrPos, ?Res)
% CurrPos is the current board in hex format
heuristic(CurrPos, Res) :-
    manhattan(CurrPos, M),
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, R, N, 1,NewN,"R"),
    linear(CurrPos, 0, C, N, 1,NewN,"C"),
    NewR is 2*R,
    NewC is 2*C,
    assert(originalR(NewR)),
    assert(originalC(NewC)),
    Res is M+NewR+NewC,
    !.

% Stop is used to define the case base
% When I reach N+1 the linear rule needs to stop.
linear(_, Temp,Lc,_,Stop,Stop,_):-Lc is Temp.
% The following two rules are the linear conflict,
% the first is for the rows, the second one is for the columns
linear(CurrPos, TempLc,Lc, N, I,Stop,Type):-
    Type = "R",
    I=<(N+1),
    % Obtain the I-th row
    Partial is CurrPos >> (8 * N * I),
    PartialSecond is Partial << (8 * N * I),
    PartialThird is CurrPos - PartialSecond,
    Row is PartialThird >> (8 * N * (I-1)),
    Idx is N,
    J is N-I,
    assert(getI(Idx)),
    assert(getIdx(Idx)),
    %Check conflicts of the row
    conflict(Row, J, Type, 0, 0, -1, R),
    retractall(getI(_)),
    retractall(getIdx(_)),
    NewI is I+1,
    NewTempLc is TempLc + R,
    linear(CurrPos, NewTempLc, Lc, N, NewI,Stop,Type).

linear(CurrPos, TempLc,Lc, N, I,Stop,Type):-
    Type="C",
    I=<(N+1),
    J is N-I,
    NewIdx is I-1,
    %Find the NewIdx-th (I-1) column
    findColumn(CurrPos,-1,0,Col,_,NewIdx),
    retractall(getI(_)),
    retractall(getIdx(_)),
    assert(getI(N)),
    assert(getIdx(N)),
    %Check conflicts of the column
    conflict(Col, J, Type, 0, 0, -1, C),
    NewI is I+1,
    NewTempLc is TempLc + C ,
    linear(CurrPos, NewTempLc, Lc, N, NewI,Stop,Type).

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
 
%Rules to check if a value is in its row/column target
isTarget(X1,_,J,Type):-
    Type == "R",
    X1 == J,!.
isTarget(_,Y1,J,Type):-
    Type == "C",
    Y1 == J.


checkMove(up,CurrPos,Result,_,LastLCCol,NewLastLCRow,NewLastLCCol):-
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, R, N, 1,NewN,"R"),
    Result is ((2*R) + LastLCCol),
    NewLastLCRow is (2*R),
    NewLastLCCol is LastLCCol,!.
checkMove(down,CurrPos,Result,_,LastLCCol,NewLastLCRow,NewLastLCCol):-
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, R, N, 1,NewN,"R"),
    Result is ((2*R) + LastLCCol),
    NewLastLCRow is (2*R),
    NewLastLCCol is LastLCCol,!.
checkMove(left,CurrPos,Result,LastLCRow,_,NewLastLCRow,NewLastLCCol):-
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, C, N, 1,NewN,"C"),
    Result is ((2*C) + LastLCRow),
    NewLastLCCol is (2*C),
    NewLastLCRow is LastLCRow,!.
checkMove(right,CurrPos,Result,LastLCRow,_,NewLastLCRow,NewLastLCCol):-
    board(_,N),
    NewN is N+1,
    linear(CurrPos, 0, C, N, 1,NewN,"C"),    
    Result is ((2*C) + LastLCRow),
    NewLastLCCol is (2*C),
    NewLastLCRow is LastLCRow,!.

% Board --> Hex of the board
% Current --> Index of the current row/column
% I --> used to iterate through all values of the hex
% Type --> C / R 
% Temp --> temporary maximum LC found
% Result --> Used to give the final result
% Value --> Value to check with other in the Hex
% IdxValue --> Idx in the Hex of the Value
conflict(Board, Current, Type, Temp, TempMax, Value, Result):-
    getI(I),
    I > 0,
    Value = -1,
    retractall(getIdx(_)),
    assertz(getIdx(I)),
    TempValue is Board >> (8 * (I-1)),
    NewValue is (TempValue mod 16),
    board(_,N),
    Dim is N*N,
    NewI is I-1,
    retractall(getI(_)),
    assertz(getI(NewI)),
    NewValue =\= Dim mod 16,
    CheckValue is NewValue-1,
    divmod(CheckValue,N,X1,Y1),
    isTarget(X1,Y1,Current,Type),
    conflict(Board, Current, Type, Temp, TempMax, NewValue, Result).

conflict(Board, Current, Type, Temp, TempMax, Value, Result):-
    getI(I),
    I > 0,
    Value =\= -1,
    TempVal is Board >> (8 * (I-1)),
    NewValue is (TempVal mod 16),
    board(_,N),
    NewI is I-1,
    retractall(getI(_)),
    assertz(getI(NewI)),
    Dim is N*N,
    NewValue =\= (Dim mod 16),
    CheckValue is NewValue-1,
    divmod(CheckValue,N,X1,Y1),
    isTarget(X1,Y1,Current,Type),
    Value > NewValue,
    %write("BOARD: "+Board+" "+Type+" : "+Current+" CONFLITTO TRA "+Value+" E "+ NewValue+"\n"),
    NewTemp is Temp+1,
    conflict(Board, Current, Type, NewTemp, TempMax, Value, Result).

conflict(Board, Current, Type, Temp, TempMax, Value, Result):-
    getI(I),
    I = 0,
    Temp>TempMax,
    conflict(Board, Current, Type, Temp, Temp, Value, Result).


conflict(Board, Current, Type, _, TempMax, _, Result):-
    getI(I),
    I = 0,
    getIdx(IdxValue),
    IdxValue =\= 1,
    retractall(getI(_)),
    NewI is IdxValue-1,
    assertz(getI(NewI)),
    conflict(Board, Current, Type, 0, TempMax, -1, Result).

conflict(_, _, _, _, TempMax, _, Result):- getIdx(IdxValue), IdxValue = 1, Result is TempMax.
conflict(Board, Current, Type, Temp, TempMax, Value, Result):-conflict(Board, Current, Type, Temp, TempMax, Value, Result).

