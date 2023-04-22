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
linear(CurrPos, Temp,Lc,N,Stop,Stop):-Lc is Temp.
linear(CurrPos, TempLc,Lc, N, I,Stop):-
    %writeln(CurrPos+","+TempLc+","+Lc+","+N+","+I),
    %writeln("NEW LINEAR"),
    I=<(N+1),
    Partial is CurrPos >> (8 * N * I),
    PartialSecond is Partial << (8 * N * I),
    Row is CurrPos - PartialSecond,
    Idx is N - 1,
    J is N-I,
    conflict(Row, J, Idx, R, "R",0, -1,Idx),
    NewIdx is I-1,
    findColumn(CurrPos,-1,0,Col,Temp,NewIdx),
    %CONTROLLARE Idx o NewIdx
    %trace,
    conflict(Col,J, Idx, C, "C", 0, -1,Idx),
    %nodebug,
    NewI is I+1,
    %writeln(C+","+R+"MEH"),
    NewTempLc is TempLc + C + R,
    %writeln("LC "+Lc),
    linear(CurrPos, NewTempLc, Lc, N, NewI,Stop).
%conflict(_,0,-2,Result,_,_,_,_):-write(Result).
%Hex esadecimale della riga/colonna da controllare
%J Jolly -> indica la riga/colonna che si sta controllando
% I indice che decrementa per scorrere tutti i valori
% Result variabile risultato
% Type --> C o R
% Temp variabile temporanea del risultato
% Val valore attuale che si sta vedendo
% IdxVal indice del valore che si sta controllando
conflict(_,_,(-1),Result,_,Temp,_,_):-Result is Temp,!.
conflict(Hex, J, I, Result, Type, Temp, Val, IdxVal):-
    %writeln(Hex+" "+J+" "+I+" "+Result+" "+Type+" "+Temp+" "+Val+" "+IdxVal),  
    %write("ENTRA1"),
    Val = -1,
    I >= 0,
    NewHex is Hex >> (8 * I),
    NewVal is (NewHex mod 16),
    NewVal =\= 0,
    CurrVal is NewVal - 1,
    %POS DEL VALORE
    board(_,N),
    divmod(CurrVal,N,X1,Y1),
    isTarget(X1,Y1,J,Type),
    NewI is I-1,
    conflict(Hex, J,NewI, Result, Type, Temp, NewVal,IdxVal).

% QUESTO FALLISCE SE
% I < 0 quindi si è finito di scorrere i numeri
% isTarget fallisce
conflict(Hex, J,I, Result, Type, Temp, Val,IdxVal):-
    %write("ENTRA2"),
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

conflict(Hex,J,I,Result,Type, Temp, Val, IdxVal):-
    %write("ENTRA3"),
    I>=0,
    NewI is I-1,
    conflict(Hex,J,NewI,Result,Type,Temp,Val, IdxVal).
conflict(Hex,J,I,Result,Type,Temp,Val,IdxVal):-
    %write("ENTRA4"),
    I<0,
    I>(-1),
    board(_,N),
    NewN is N-1,
    IdxVal=NewN,
    NewIdxVal is IdxVal-1,
    Result is Temp,
    conflict(Hex,J,(N-2),Result,Type,0,-1,NewIdxVal).        
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
%conflict(Hex,J,I,Result,Type,Temp,Val,IdxVal):-
%    write("ENTRA6"),
%    I<0,
%    I>(-1),
%    NewIdxVal is IdxVal-1,
%  NewI is NewIdxVal-1,
%    conflict(Hex,J,NewI,Result,Type,0,-1,NewIdxVal).

isTarget(X1,_,J,Type):-
    Type == "R",
    X1 == J,!.


isTarget(_,Y1,J,Type):-
    Type == "C",
    Y1 == J.


findColumn(_,N,N,Col,Temp,Idx):- Col is Temp.
findColumn(CurrPos,-1,I,Col,Temp,Idx):-
    board(_,N),
    NewTemp is 0,
    findColumn(CurrPos,N,I,Col,NewTemp,Idx),!.
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
    