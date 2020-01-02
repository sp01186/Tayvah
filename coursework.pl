:- use_module(library(lists)).


/** Q3.1  */


means(hate, haine).
means(fear, peur).
means(brave, corageux).
means(darkness, obscurite).
means(anger, colere).
means(focus, concentrer).
means(power, puissance).
means(strength, force).
means(submit, soumettre).
means(is, est).
means(mad, fou).
means(X, X).

translate_eng([],[]).

translate_eng([X|Xs], [Y|Ys]) :-
	means(X, Y),
	translate_eng(Xs, Ys).



/** Q3.2 */

edge(c1, c2, 4).
edge(c1, c5, 2).
edge(c1, c3, 10).
edge(c3, c5, 2).
edge(c3, c4, 1).
edge(c5, c4, 8).
edge(c2, c4, 6).
edge(c2, c5, 1).
edge(c2, c6, 1).

connected(X,Y, L) :- edge(X, Y, L) ; edge(Y,X,L).

path(A, B, Path, Length) :- 
	travel(A, B, [A], Q, Length),
	rev(Q, Path).



travel(A,B,P,[B|P], L) :-
	connected(A,B,L).

travel(A,B,Visited,Path,L)  :-
	connected(A,C,D),
	C \= B,
	\+member(C, Visited),
	travel(C,B, [C|Visited], Path, L1),
	L is D+L1.



shortest(A,B,Path,Length) :-
   setof([P,L],path(A,B,P,L),Set),
   Set = [_|_], % fail if empty
   minimal(Set,[Path,Length]).

minimal([F|R],M) :- min(R,F,M).

% minimal path
min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min). 
min([_|R],M,Min) :- min(R,M,Min).

/* rev(List, Reverse):-
	rev(List, [], Reverse).
	
rev([], Reverse, Reverse). !.
rev([H|Tail], Reverse0, Reverse):-
	rev(Tail, [H|Reverse0], Reverse).   */

/** Q3.3     */

rabbit(X) :-
	sol(Street),
	member(house(_,X,rabbit), Street).
	

sol(Street) :- 
	Street = [H1, H2, H3], % Street of 3 houses


	H1 = house(Col1, Nat1, Pet1),
	H2 = house(Col2, Nat2, Pet2),
	H3 = house(Col3, Nat3, Pet3),

	% facts

	member(house(brown, _, _), Street),
	member(house(blue, _, _), Street),	
	member(house(black, french, _), Street),
	member(house(_,spanish,jaguar), Street),
	member(house(_,_,snail), Street),
	rightof(house(_,_,snail), house(_, japanese, _), Street),
	leftof(house(blue, _, _), house(_, _, snail), Street),
	member(house(_,_,rabbit), Street),


	% permutations
	permutation([black, blue, brown], [Col1, Col2, Col3]),
	permutation([french, spanish, japanese], [Nat1, Nat2, Nat3]),
	permutation([jaguar, snail, rabbit], [Pet1, Pet2, Pet3]).


	






% check left of
leftof(L, R, [L,R|_]).
leftof(L, R, [_ | Rest]) :- leftof(L, R, Rest).



% check right of
rightof(R, L, [L,R|_]).
rightof(R, L, [_ | Rest]) :- rightof(R, L, Rest).






	
