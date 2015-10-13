proc3([],[]).   % base case: an empty list has no duplicate elements, thus no duplicate elements in either list
proc3([H1|T1],L2) :- member(H1,T1),proc3(T1,L2).    % recursion: remove the head of L1, if it is a member of the rest of L1, and continue recursively using L2 and the remaining L1
proc3([H1|T1],[H1|T2]) :- \+(member(H1,T1)),proc3(T1,T2).   % recursion: if the head of both lists are the same, and if the head is not a member of the rest of L1, continue recursively using the remaining L1 and remaining L2.
