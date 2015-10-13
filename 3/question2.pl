word(dog,3,[d,o,g]).
word(run,3,[r,u,n]).
word(top,3,[t,o,p]).
word(five,4,[f,i,v,e]).
word(four,4,[f,o,u,r]).
word(lost,4,[l,o,s,t]).
word(mess,4,[m,e,s,s]).
word(unit,4,[u,n,i,t]).
word(baker,5,[b,a,k,e,r]).
word(forum,5,[f,o,r,u,m]).
word(green,5,[g,r,e,e,n]).
word(super,5,[s,u,p,e,r]).
word(prolog,6,[p,r,o,l,o,g]).
word(vanish,6,[v,a,n,i,s,h]).
word(wonder,6,[w,o,n,d,e,r]).
word(yellow,6,[y,e,l,l,o,w]).

%puzzle([
%    [1,across,5],
%    [3,across,6],
%    [1,down,4],
%    [3,down,3],
%    [5,down,4]
%]).

vocab([
    [d,o,g],
    [r,u,n],
    [t,o,p],
    [f,i,v,e],
    [f,o,u,r],
    [l,o,s,t],
    [m,e,s,s],
    [u,n,i,t],
    [b,a,k,e,r],
    [f,o,r,u,m],
    [g,r,e,e,n],
    [s,u,p,e,r],
    [p,r,o,l,o,g],
    [v,a,n,i,s,h],
    [w,o,n,d,e,r],
    [y,e,l,l,o,w]
]).

solution(Puzzle,Vocab,Solution).

% for each word in vocab,
%   check length of word
%       length must fit in size
%   check value at number
%       for each other number
%           check if start = value
%           check length of word
%               length must fit in size
%           recurse.

cross_word(P) :-
