% here's also one way of solving this puzzle. 
% other ways should also be fairly similar though, in that it needs to have one big conjuction of all the constraints
% and some kind of dataset for the floors.

floors([floor(_,5),floor(_,4),floor(_,3),floor(_,2),floor(_,1)]).
building(Floors) :- floors(Floors), member(
	floor(yves,Y),Floors), Y\=5, member(
	floor(dave,D),Floors), D\=1, member(
	floor(brian,B),Floors),B\=1,B\=5,member(
	floor(ed,E),Floors),E>D, member(
	floor(mike,M),Floors), not(adjacent(B,M)), not(adjacent(B,D)).

adjacent(M,N) :- M is N+1; M is N-1.

/* output

?- building(F).
F = [floor(ed, 5), floor(brian, 4), floor(yves, 3), floor(dave, 2), floor(mike, 1)] ;
false.

*/