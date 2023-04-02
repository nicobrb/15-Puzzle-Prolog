:-['dominio.pl', 'azioni.pl','./traversal_strategy/iterative_deepening_astar.pl'].

prova(Soluzione) :- 
    is_solvable(X),
    X = 1,
    board(List,N),
    nth0(BlankPos,List,v),
    Max is N*N,
    replace(List,BlankPos,Max,NewList),
    hex_bytes(Hex,NewList),
    fromHexToInteger(Hex,StartingBoard),
    ida(StartingBoard, BlankPos, Soluzione),
    write(Soluzione).


