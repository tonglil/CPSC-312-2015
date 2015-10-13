% base case: a list combined with an empty list is the same list
proc1(L1,[],L1).
% base case: the other order of list
proc1([],L2,L2).
% recursion: prepend the heads of L1 and L2 onto a list, and continue recursively using the remaining L1, L2, and that list
proc1([H1|T1],[H2|T2],[H1,H2|T3]) :- proc1(T1,T2,T3).



% base case: empty lists are duplicates of each other
proc2a([],[]).
% recursion: duplicate and prepend the head of L1 onto a list, and continue recursively using the remaining L1 and that list
proc2a([H1|T1],[H1,H1|T2]) :- proc2a(T1,T2).



% base case: empty list L2 has no element, therefore L1 is empty as well
proc2b([],[]).
% recursion: pop the head of L1 and check membership of L2, if so, remove all occurrences of it from L2, and continue recursively with the resulting list and the remaining L1
proc2b([H1|T1],L2) :- member(H1,L2),remove(H1,L2,X),proc2b(T1,X).

% the base case: no occurrences of a variable is in an empty list
remove(_,[],[]).
% recursion: remove X if it is the head of L1, and continue recursively using the remaining L1 and L2
remove(X,[X|T1],L2) :- remove(X,T1,L2).
% recursion: if X is not the same as H1, recursively remove X from inside of the remaining L1, and prepend H1 onto L2 (the resulting list of that operation)
remove(X,[H1|T1],[H1|L2]) :- \+(same(X,H1)),remove(X,T1,L2).

% base case: true if two variables are the same
same(X,X).



% base case: an empty list has no duplicate elements, thus no duplicate elements in either list
proc3([],[]).
% recursion: remove the head of L1, if it is a member of the rest of L1, and continue recursively using L2 and the remaining L1
proc3([H1|T1],L2) :- member(H1,T1),proc3(T1,L2).
% recursion: if the head of both lists are the same, and if the head is not a member of the rest of L1, continue recursively using the remaining L1 and remaining L2.
proc3([H1|T1],[H1|T2]) :- \+(member(H1,T1)),proc3(T1,T2).



% base case: an empty list has no elements, thus each list is the permutation of the other and contain the exact same elements (none)
proc4([],[]).
% recursion: remove the head of L1, and continue recursively using the remaining L1 and a list where the head of L1 has been deleted from somewhere in L2
proc4([H1|T1],L2) :- proc4(T1,X),delete(H1,L2,X).

% base case: remove X if it is the head of L1, and the element has been removed from the list
delete(X,[X|T1],T1).
% recursion: remove the head of L1, recursively remove X from the remaining L1, and prepend the removed head of L1 to L2.
delete(X,[H1|T1],[H1|L2]) :- delete(X,T1,L2).
