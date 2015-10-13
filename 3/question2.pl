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

solution(A1,A3,D1,D3,D5) :-
    word(A1,A,_,B,_,C),
    word(A3,D,_,E,_,F,_),
    word(D1,A,_,D,_),
    word(D3,B,_,E),
    word(D5,C,_,F,_).
