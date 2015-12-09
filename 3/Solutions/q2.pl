% here's one way of solving this puzzle
% all other solutions should be run and checked for correctness


word(d,o,g).
word(r,u,n).
word(t,o,p).
word(f,o,u,r).
word(l,o,s,s).
word(m,e,s,s).
word(f,i,v,e).
word(u,n,i,t).
word(b,a,k,e,r).
word(f,o,r,u,m).
word(g,r,e,e,n).
word(s,u,p,e,r).
word(p,r,o,l,o,g).
word(v,a,n,i,s,h).
word(w,o,n,d,e,r).
word(y,e,l,l,o,w).

solutions(L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16) :- 
	   word(L1,L2,L3,L4,L5	% 1 across
	), word(L9,L10,L11,L12,L13,L14	% 3 across
	), word(L1,L6,L9,L15			% 1 down
	), word(L3,L7,L11				% 3 down
	), word(L5,L8,L13,L16).	% 5 down

% solution:
% 1 across: forum
% 3 across: vanish
% 1 down: five
% 3 down: run
% 5 down: mess
