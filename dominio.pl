% Ground truth of 'sliding block puzzle'.
% Modify the predicate board(+Board, +Dimension) to set the starting state of the problem. 
% The rest of the domain is parametric, no need to modify it.
board([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,v], 3).
:- initialization(generate_end_position).

% board dimension
dim(N) :- board(_, N).
% starting position
start_position(S) :- board(S, _).
% goal position
generate_end_position :-
    retractall(end_position(_)),
    dim(N),
    L is N*N - 1,
    findall(X, (between(1, L, X)), S0),
    append(S0, ['v'], S),
    assert(end_position(S)).

% If N % 2 == 1
solvable(Board) :-
    dim(N),
    1 =:= N mod 2,
    count_inversions(Board, InvCount),
    % If inversion_count(board) % 2 == 0
    0 =:= InvCount mod 2, !.
% If N % 2 == 0
solvable(Board) :- 
    dim(N),
    0 =:= N mod 2,
    count_inversions(Board, InvCount),
    empty_pos(Board, EmptyIdx),
    % If (pos(v) % 2 == 0 and inversion_count(board) % 2 == 1)
    % or (pos(v) % 2 == 1 and inversion_count(board) % 2 == 0)
    1 is (InvCount + EmptyIdx) mod 2, !.

empty_pos(Board, Index) :-
    dim(N),
    nth0(I, Board, v),
    Index is I div N, !.

pred(X, Y) :- 
    number(Y), number(X), Y < X.
pred(X, _) :- 
    number(X), Y1 is 0, Y1 < X, Y1 > 0.
pred(_, Y) :- 
    number(Y), X1 is 0, Y < X1.

count_inversions([], 0).
count_inversions([H|T], R) :-
    include(pred(H), T, Filtered),
    length(Filtered, InversionCount),
    count_inversions(T, R1),
    R is InversionCount + R1.
