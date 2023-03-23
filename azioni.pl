applicabile(up, _):-
	posV(V), board(_,N), V-N>=0.
applicabile(down, _):-
	posV(V), board(_,N), V+N<N*N.
applicabile(left, _):-
	posV(V), board(_,N), (V mod N) \==0. 
applicabile(right, _):-
	posV(V), board(_,N), (V mod (N-1)) \==0.

trasforma(up, S0, S1):-
	posV(V), board(_,N), NewPos is V-N, 
	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
	retractall(board(_,_)),assert(board(S1,N)).
trasforma(down, S0, S1):-
	posV(V), board(_,N), NewPos is V+N, 
	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
	retractall(board(_,_)),assert(board(S1,N)).
trasforma(left, S0, S1):-
	posV(V), board(_,N), NewPos is V-1, 
	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
	retractall(board(_,_)),assert(board(S1,N)).
trasforma(right, S0, S1):-
	posV(V), board(_,N), NewPos is V+1, 
	swap(V,NewPos,S0,S1),retractall(posV(_)),assert(posV(NewPos)),
	retractall(board(_,_)),assert(board(S1,N)).

% To test this command use:
% board(S0,_),trasforma(left,S0,S1),write(S0),write(S1).
swap(V,NewPos,S0,S1):-
	% Create a list of V blank elements
	length(L1, V),
	% Create a list of NewPos blank elements
	length(L2, NewPos),
	% In L1 put the list 
	% (there will be all the elements before the one to swap)
	% Head1 contains the element to swap
	% Tail1 contains the remains elements of the list
	append(L1, [Head1|Tail1], S0),
	% Put in Temp the first and second part of the list
	% between the two parts put a variable
	append(L1, [Head2|Tail1], Temp),
	% Put in L2 the first part of the list without
	% the element to replace
	% Head2 contains the elements who will be replaced
	% Tail2 contains the second part of the list
	append(L2, [Head2|Tail2], Temp),
	append(L2, [Head1|Tail2], S1).