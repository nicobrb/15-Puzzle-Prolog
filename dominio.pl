[library(dcg/basics)].

% Ground truth of 'sliding block puzzle'.
% Modify the predicate board(+Board, +Dimension) to set the starting state of the problem. 
% The rest of the domain is parametric, no need to modify it.
%   board([v,4,12,14,13,2,8,11,9,1,3,5,15,6,7,10], 4).
%   board([1,2,3,4,5,6,7,8,9,10,12,v,13,14,11,15], 4). % funziona.
%   board([1,2,3,4,5,6,7,8,9,v,10,12,13,14,11,15], 4). %funziona.
%   board([1,2,3,4,5,6,7,8,9,14,10,12,13,v,11,15], 4). %funziona.
%   board([1,2,3,4,5,6,7,8,9,10,15,11,13,14,v,12], 4). %funziona.
%   board([1,v,2,4,5,6,3,8,9,10,7,11,13,14,15,12], 4).
%   board([5,1,3,4,9,2,11,7,v,6,10,8,13,14,15,12], 4). %scramble da 10 mosse
%   board([1,2,7,3,5,10,6,4,9,12,11,8,13,v,14,15], 4). % scramble 12 moves
%  board([v,2,7,3,1,10,6,4,5,9,11,8,13,12,14,15], 4). % scramble 16 mosse
%   board([8,6,7,2,5,4,3,v,1], 3). % hardest 8 puzzle, 31 moves
%   board([1,8,2,4,v,3,7,6,5], 3). % 9 moves
% board([7,1,3,6,2,v,4,5,8], 3).
%   board([14,9,10,6,5,1,8,2,7,12,3,15,11,13,4,v], 4).
%   board([3,v,4,8,1,10,2,6,5,13,7,12,14,9,11,15], 4).
board([
     1, 10, 15,  4,
    13,  6,  3,  8,
     2,  9, 12,  7,
    14,  5,  v, 11], 4). % 35 mosse
%board([
%     4,  3,  1,  2,
%     5,  6,  7,  8,
%     9, 10, 11, 12,
%    13, 14, 15,  v], 4). % EXAMPLE BOARD
%board([v,2,1,
%       7,4,5,
%       6,3,8],3).
%board([1,10,15,4,
%      13,6,3,8,
%      2,9,12,7,
%      14,5,v,11],4).
%board([13, 14, 15, 12,
%        9, 10, 11,  8,
%        5,  6,  7,  4,
%        1,  2,  3,  v],4).
% board([9,14,7,13,6,4,5,3,15,11,v,1,10,2,8,12], 4).
%board([v,12,9,13,15,11,10,14,3,7,2,5,4,8,6,1], 4). % 80 mosse
:-dynamic(board/2).
% board dimension
dim(N) :- board(_, N).
is_solvable(0).
:-dynamic(is_solvable/1).
% starting position
start_position(S) :- board(S, _).
% generation of goal position: gen_list_solution generates a list of sorted integers ranging from 1 to N-1, then the blank "v" symbol
% is appended. 

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).


generateGoal:-
    board(_,N),
    Max is N*N,
    Position is Max-1,
    calculateGoal(Solution, Position, Max),
    retractall(goal(_)),
    assert(goal(Solution)),
    !.

calculateGoal(Max, 0, Max).
    
calculateGoal(Solution, Position, Max):-
    NewPos is Position-1,
    calculateGoal(NewSolution, NewPos, Max),
    Solution is NewSolution + ((Max-Position)* (1<<(8*Position))).

:-generateGoal.

% If N is odd
solvable:-
    board(Board, N),
    1 =:= N mod 2,
    count_inversions(Board, InvCount),
    % If inversion_count(board) % 2 == 0
    0 =:= InvCount mod 2, !,
    retractall(is_solvable(_)),
    assert(is_solvable(1)).

% If N is even
solvable:-
    board(Board, N), 
    0 =:= N mod 2,
    count_inversions(Board, InvCount),
    empty_pos(Board, EmptyIdx),
    1 is (InvCount + EmptyIdx) mod 2,!,
    retractall(is_solvable(_)),
    assert(is_solvable(1)).

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

fromHexToInteger(H, N) :-
    atom_concat('0x', H, HexaAtom),
    atom_codes(HexaAtom, HexaCodes),
    number_codes(N, HexaCodes).

fromIntegerToHex(N, H):- 
    phrase(xinteger(N), HexaCodes),
    atom_codes(H, HexaCodes). 

:-solvable.

