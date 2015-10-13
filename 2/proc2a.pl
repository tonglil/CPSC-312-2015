proc2a([],[]).  % base case: empty lists are duplicates of each other
proc2a([H1|T1],[H1,H1|T2]) :- proc2a(T1,T2).    % recursion: duplicate and prepend the head of L1 onto a list, and continue recursively using the remaining L1 and that list
