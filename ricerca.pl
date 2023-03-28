% listaRisolto([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
% hexaRisolto('0102030405060708090a0b0c0d0e0f10').
% listaScrambled([1,2,8,4,5,6,7,16,9,10,11,12,14,13,15,3]).
% hexaScrambled('0102080405060710090a0b0c0e0d0f03'). %1339775167246176754970653633178046211.
% listaSx([1,2,8,4,5,6,16,7,9,10,11,12,14,13,15,3]). %1339775167246219090248302796599004931.
% hexaSx('0102080405061007090a0b0c0e0d0f03'). %'0102080405061007090a0b0c0e0d0f03', python = 0102080405061007090a0b0c0e0d0f03
% listaDx([1,2,8,4,5,6,7,9,16,10,11,12,14,13,15,3]).
% hexaDx('0102080405060709100a0b0c0e0d0f03'). %0102080405060709100a0b0c0e0d0f03
% listaUp([1,2,8,16,5,6,7,4,9,10,11,12,14,13,15,3]).
% hexaUp('0102081005060704090a0b0c0e0d0f03'). %0102081005060704090a0b0c0e0d0f03
% listaDown([1,2,8,4,5,6,7,12,9,10,11,16,14,13,15,3]).
% hexaDown('010208040506070c090a0b100e0d0f03'). %010208040506070c090a0b100e0d0f03
%:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:-['dominio.pl'].
:-['azioni.pl'].


initialize:- retractall(depth(_)),
            assert(depth(1)).

prova(Soluzione) :- 
    board(List,N), 
    %solvable(List),
    depth(Depth),
    nth0(BlankPos,List,v),
    Max is N*N,
    replace(List,BlankPos,Max,NewList), % da cambiare se vogliamo fare giochi diversi da quello del 
    hex_bytes(Hex,NewList),
    fromHexToInteger(Hex,StartingBoard),
    iterativeDeepening(StartingBoard, BlankPos, Depth, Soluzione).

prova(Soluzione):-
    depth(Depth),
    NewDepth is Depth+1,
    retractall(depth(_)),
    assert(depth(NewDepth)),
    prova(Soluzione).

iterativeDeepening(StartingBoard,BlankPos, Depth, Soluzione):-
    write('---- Depth: '), write(Depth),write('\n'),
    nextMove(StartingBoard,Solution,BlankPos, V, Depth).


nextMove(Position, [], BlankPos, LastMove, MaxDepth):-
    goal(Solution), 
    Position = Solution,
    !,
    depth(Depth),
    write('Solution found at depth '), write(Depth),write('!\n').


nextMove(Position,[Move|MoveList],BlankPos,LastMove,MaxDepth):-
    MaxDepth > 0,
    %inverse(Move,Inverse),
    applicabile(Move,BlankPos), 
    %notMember(Move, [LastMove|PreviousTwoMoves]),
    trasforma(Position,Move,BlankPos,NewPosition,NewBlankPos),
    % popAndAppend([LastMove|PreviousTwoMoves],Inverse,LastMoves),
    NewDepth is MaxDepth-1,
    nextMove(NewPosition,MoveList,NewBlankPos,Move,NewDepth).


