% Sample solutions for assignment 4 
% Obviously, these are only one way to solve these problems and not the only way
% but they are relatively efficint ways.
% 

% Problem 1

proc1(X,[],X).
proc1([],X,X).
proc1([X|L1],[Y|L2],[X,Y|L3]) :- proc1(L1,L2,L3).

% note: [X,Y|L3] can also be written as [X|[Y|L3]]

/* outputs:
?- proc1([a,b,c],[d,e,f],[a,d,b,e,c,f]).
true ;
true.

?- proc1([a,b],[d,e,f,g],[a,d,b,e,f,g]).
true.

?- proc1(X,Y,[1,2,3,4,5,6]).
X = [1, 2, 3, 4, 5, 6],
Y = [] ;
X = [],
Y = [1, 2, 3, 4, 5, 6] ;
X = [1, 3, 4, 5, 6],
Y = [2] ;
X = [1],
Y = [2, 3, 4, 5, 6] ;
X = [1, 3, 5, 6],
Y = [2, 4] ;
X = [1, 3],
Y = [2, 4, 5, 6] ;
X = [1, 3, 5],
Y = [2, 4, 6] ;
X = [1, 3, 5],
Y = [2, 4, 6] ;
false.

*/

% Problem 2a

proc2a([],[]).
proc2a([H|T1],[H,H|T2]) :- proc2a(T1,T2).

/* outputs
?- proc2a([a,b,c],[a,a,b,b,c,c]).
true.

?- proc2a([a,b,c],X).
X = [a, a, b, b, c, c].

?- proc2a([a,b,c],[a,b,c,a,b,c]).
false.
*/

% Problem 2b

proc2b([],[]).
proc2b([X|T],L) :- member1(X,L), del1(X,L,L2), proc2b(T,L2).

member1(X, [X|_]).
member1(X,[H|T]) :- X\=H, member1(X,T).

del1(_,[],[]).
del1(X,[X|T],L) :- del1(X,T,L).
del1(X,[H|T],[H|L]) :- X\=H, del1(X,T,L).

/* outputs
?- proc2b(X,[a,b,a,b,c,c,c]).     
X = [a, b, c] ;
false.

?- proc2b([a,b,c],[a,b,a,b,c,c,c]).
true ;
false.
*/

% Problem 3

proc3([],[]).
proc3([X|Xs],Ys) :- member1(X,Xs),proc3(Xs,Ys).
proc3([X|Xs],[X|Ys]) :- not(member1(X,Xs)),proc3(Xs,Ys).

/* outputs
?- proc3([a,b,c,b,d,b],[a,c,d,b]).
true ;
false.

?- proc3([a,b,c,b,d,b],X).
X = [a, c, d, b] ;
false.

*/

% Problem 4

proc4([],[]).
proc4([H1|T1],L) :- proc4(T1,L1),del2(H1,L,L1).

del2(X,[X|T],T).
del2(X,[Y|T],[Y|T1]) :- del2(X,T,T1).

/*

?- proc4([a,b,c],[b,c,a]).
true ;
false.

?- proc4([a,b,c],[a,c,b]).
true ;
false.

?- proc4([a,b,c],[c,b,a]).
true ;
false.

?- proc4([a,b,c],X).
X = [a, b, c] ;
X = [b, a, c] ;
X = [b, c, a] ;
X = [a, c, b] ;
X = [c, a, b] ;
X = [c, b, a] ;
false.


*/
