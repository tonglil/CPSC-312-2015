proc2b([],[]).  % base case: empty list L2 has no element, therefore L1 is empty as well
proc2b([H1|T1],L2) :- member(H1,L2),remove(H1,L2,X),proc2b(T1,X).   % recursion: pop the head of L1 and check membership of L2, if so, remove all occurrences of it from L2, and continue recursively with the resulting list and the remaining L1

remove(_,[],[]).    % the base case: no occurrences of a variable is in an empty list
remove(X,[X|T1],L2) :- remove(X,T1,L2). % recursion: remove X if it is the head of L1, and continue recursively using the remaining L1 and L2
remove(X,[H1|T1],[H1|L2]) :- \+(same(X,H1)),remove(X,T1,L2).    % recursion: if X is not the same as H1, recursively remove X from inside of the remaining L1, and prepend H1 onto L2 (the resulting list of that operation)

same(X,X).  % base case: true if two variables are the same
