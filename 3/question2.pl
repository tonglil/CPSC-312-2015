% The set of vocabulary available to the puzzle.
%
% The representation describes the vocabulary where:
% word predicates have character length + 1 arity,
% and each vocabulary word is represented as a word predicate with the word itself and each character of the word as parameters.

word(dog,d,o,g).
word(run,r,u,n).
word(top,t,o,p).
word(five,f,i,v,e).
word(four,f,o,u,r).
word(lost,l,o,s,t).
word(mess,m,e,s,s).
word(unit,u,n,i,t).
word(baker,b,a,k,e,r).
word(forum,f,o,r,u,m).
word(green,g,r,e,e,n).
word(super,s,u,p,e,r).
word(prolog,p,r,o,l,o,g).
word(vanish,v,a,n,i,s,h).
word(wonder,w,o,n,d,e,r).
word(yellow,y,e,l,l,o,w).

% The crossword.
%
% The representation describes each "mini-puzzle" in the crossword using words where:
% all of the mini-puzzles are passed into the solution procedure,
% each mini-puzzle is a query for a word,
% each box in the mini-puzzle is represented as a parameter in a word predicate,
% if a mini-puzzle intersects with another one, the box is represented as the same variable in the parameters of both queries,
% otherwise each box is represented as _ (any variable) in the parameters.

solution(A1,A3,D1,D3,D5) :-
    word(A1,A,_,B,_,C),
    word(A3,D,_,E,_,F,_),
    word(D1,A,_,D,_),
    word(D3,B,_,E),
    word(D5,C,_,F,_).

% The output.
%
% The representation of each solution is given where:
% the first letter stands for the direction (A = across, D = down),
% the number stands for the location on the puzzle,
% and the value they contain stands for the word that is the solution.
%
% ?- puzzle(A1,A3,D1,D3,D5).
% A1 = forum,
% A3 = vanish,
% D1 = five,
% D3 = run,
% D5 = mess ;
% false.
