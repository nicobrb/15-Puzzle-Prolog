% Ground truth of 'sliding block puzzle'.
% Modify the predicate board(+Board, +Dimension) to set the starting state of the problem. 
% The rest of the domain is parametric, no need to modify it.
board([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,v], 4).
:- initialization(generate_goal_position).
:-dynamic(board/2).
% board dimension
dim(N) :- board(_, N).
% starting position
start_position(S) :- board(S, _).
% generation of goal position: gen_list_solution generates a list of sorted integers ranging from 1 to N-1, then the blank "v" symbol
% is appended. 
generate_goal_position:- 
    retractall(goal(_)),
    dim(N),
    L is N*N - 1,
    board(B,_),
    nth0(I, B, v),
    assert(posV(I)),
    gen_list_solution(1, L, X),
    append(X, ['v'], S),
    assert(goal(S)).

gen_list_solution(X,X,[X]) :- !.
gen_list_solution(X,Y,[X|Xs]) :-
    X =< Y,
    Z is X+1,
    gen_list_solution(Z,Y,Xs).


% generate_goal_position :-
%     retractall(goal(_)),
%     dim(N),
%     L is N*N - 1,
%     findall(X, (between(1, L, X)), S0),
%     append(S0, ['v'], S),
%     assert(goal(S)).

% If N is odd
solvable(Board) :-
    dim(N),
    1 =:= N mod 2,
    count_inversions(Board, InvCount),
    % If inversion_count(board) % 2 == 0
    0 =:= InvCount mod 2, !.
% If N is even
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
    integer(Y), integer(X), Y < X.
pred(X, _) :- 
    integer(X), Y1 is 0, Y1 < X.
pred(_, Y) :- 
    integer(Y), X1 is 0, Y < X1.

count_inversions([], 0).
count_inversions([H|T], R) :-
    include(pred(H), T, Filtered),
    length(Filtered, InversionCount),
    count_inversions(T, R1),
    R is InversionCount + R1.
