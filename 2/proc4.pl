proc4([],[]).   % base case: an empty list has no elements, thus each list is the permutation of the other and contain the exact same elements (none)
proc4([H1|T1],L2) :- proc4(T1,X),delete(H1,L2,X).   % recursion: remove the head of L1, and continue recursively using the remaining L1 and a list where the head of L1 has been deleted from somewhere in L2

delete(X,[X|T1],T1).    % base case: remove X if it is the head of L1, and the element has been removed from the list
delete(X,[H1|T1],[H1|L2]) :- delete(X,T1,L2).   % recursion: remove the head of L1, recursively remove X from the remaining L1, and prepend the removed head of L1 to L2.
