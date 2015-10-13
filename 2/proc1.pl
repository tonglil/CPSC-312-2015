proc1(L1,[],L1).    % base case: a list combined with an empty list is the same list
proc1([],L2,L2).    % base case: the other order of list
proc1([H1|T1],[H2|T2],[H1,H2|T3]) :- proc1(T1,T2,T3).   % recursion: prepend the heads of L1 and L2 onto a list, and continue recursively using the remaining L1, L2, and that list
