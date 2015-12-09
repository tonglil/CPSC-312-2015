%%%%%%%%%%%%%(a & b)
dictionary([(o,o),(fortuna,fortune),(velut,like),(luna,'the moon'),(statu,youre), (variabilis,changeable),(semper,ever),(crescis,waxing),(
aut,and),(decrescis,waning),(vita,life),(detestabilis,hateful),(nunc,first),(obdurat,oppresses),(et,and),(tunc,then),(curat,soothes),(
egestatem,poverty),(potestatem,power),(dissolvit,melts),(ut,like),(glaciem,ice)]).

lookupa(X,Y) :- dictionary(D), member((X,Y),D).

translatea([],[]).
translatea([X|Xs],[Y|Ys]):- lookupa(X,Y), translatea(Xs,Ys).

% if they have dropped 'the' from moon, it should also be acceptable.
/* outputs:
?- translatea([o,fortuna],X).
X = [o, fortune] ;
false.

?- translatea([velut,luna],X).
X = [like, 'the moon'] ;
false.

?- translatea([statu,variabilis],X).
X = [youre, changeable] ;
false.

?- translatea([o,luna,statu,velut,fortuna],X).
X = [o, 'the moon', youre, like, fortune] ;
false.

?- translatea([variabilis,fortuna],X).        
X = [changeable, fortune] ;
false.

?- translatea([statu,velut,luna],X).  
X = [youre, like, 'the moon'] ;
false.

?- translatea([potestatem,nunc,curat,tunc,obdurat],X).
X = [power, first, soothes, then, oppresses] ;
false.

?- translatea([egestatem,detestabilis],X).            
X = [poverty, hateful] ;
false.

?- translatea([statu,crescis,et,decrescis],X).
X = [youre, waxing, and, waning] ;
false.

*/

%%%%%%%%%%%%%%%%%%%%%(c)
latin_sentence([o, fortuna, velut, luna, statu, variabilis, semper, crescis, aut, decrescis, vita, detestabilis, nunc, obdurat, et, tunc, curat, egestatem, potestatem, dissolvit, ut, glaciem]).
english_sentence([o, fortune, like, 'the moon', youre, changeable, ever, waxing, and, waning, life, hateful, first, oppresses, and, then, soothes, poverty, power, melts, like, ice]).

load(Dict) :- latin_sentence(L),english_sentence(E),translatec(L,E,Dict).

lookupc(X,[(X,Y)|_],Y).
lookupc(X,[(Z,_)|Dict],Y) :- X\=Z, lookupc(X,Dict,Y).

translatec([],[],_).
translatec([X|Xs],[Y|Ys],Dict) :- lookupc(X,Dict,Y), translatec(Xs,Ys,Dict).

/* outputs:

?- load(D),translatec([o,fortuna],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [o, fortune] ;
false.

?- load(D),translatec([velut,luna],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [like, 'the moon'] ;
false.

?- load(D),translatec([statu,variabilis],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [youre, changeable] ;
false.

?- load(D),translatec([o,luna,statu,velut,fotuna],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [o, 'the moon', youre, like, _G2851] .

?- load(D),translatec([o,luna,statu,velut,fortuna],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [o, 'the moon', youre, like, fortune] ;
false.

?- load(D),translatec([variabilis,fortuna],X,D).        
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [changeable, fortune] ;
false.

?- load(D),translatec([statu,velut,luna],X,D).  
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [youre, like, 'the moon'] ;
false.

?- load(D),translatec([potestatem,nunc,curat,tunc,obdurat],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [power, first, soothes, then, oppresses] ;
false.

?- load(D),translatec([egestatem,detestabilis],X,D).            
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [poverty, hateful] ;
false.

?- load(D),translatec([statu,crescis,et,decrescis],X,D).
D = [ (o, o), (fortuna, fortune), (velut, like), (luna, 'the moon'), (statu, youre), (variabilis, changeable), (semper, ever), (crescis, waxing), (..., ...)|...],
X = [youre, waxing, and, waning] ;
false.

*/

%%%%%%%%%%%%%%%%(d)

dictionary(latin,[(o,o),(fortuna,fortune),(velut,like),(luna,'the moon'),(statu,youre), (variabilis,changeable),(semper,ever),(crescis,waxing),(
aut,and),(decrescis,waning),(vita,life),(detestabilis,hateful),(nunc,first),(obdurat,oppresses),(et,and),(tunc,then),(curat,soothes),(
egestatem,poverty),(potestatem,power),(dissolvit,melts),(ut,like),(glaciem,ice)]).

dictionary(german,[(freude,joy),(
schöner,beautiful),(
götter,gods),(
funken,spark),(
tochter,daughter),(
aus,from),(
elysium,elysium),(
wir,we),(
betreten,enter),(
feuer,fire),(
trunken,drunk),(
himmlische,'heavenly being'),(
dein,your),(
heiligtum,sanctuary)]).

lookupd(X,Y,Language) :- dictionary(Language,D),member((X,Y),D).
translated([],[],_).
translated([X|Xs],[Y|Ys],Language) :- lookupd(X,Y,Language), translated(Xs,Ys,Language).

/* output

?- translated([freude,schöner,götter,funken],X,german).
X = [joy, beautiful, gods, spark] ;
false.

?- translated([tochter,aus,elysium],X,german).         
X = [daughter, from, elysium] ;
false.

?- translated([wir,betreten,feuer,trunken],X,german).
X = [we, enter, fire, drunk] ;
false.

?- translated([himmlische,dein,heiligtum],X,german). 
X = ['heavenly being', your, sanctuary] ;
false.

?- translated([velut,luna],X,latin).                
X = [like, 'the moon'] ;
false.

?- translated([dein,schöner,heiligtum],X,L).    
X = [your, beautiful, sanctuary],
L = german ;
false.

?- translated([feuer,götter,aus,elysium],X,L).
X = [fire, gods, from, elysium],
L = german ;
false.

?- translated([dein,funken],X,L).                        
X = [your, spark],
L = german ;
false.

?- translated([trunken,himmlische],X,L).
X = [drunk, 'heavenly being'],
L = german ;
false.

*/

%%%%%%%%%%%%%%%%%%(e)

lookupe(X,Y,Language) :- dictionary(Language,D),member((X,Y),D).
lookupe(X,?,Language) :- dictionary(Language,D),noentry(X,D).

noentry(X,[(K,V)|Dict]):- X\=K,noentry(X,Dict). 
noentry(_,[]).

translatee([],[],_).
translatee([X|Xs],[Y|Ys],Language) :- lookupe(X,Y,Language), translatee(Xs,Ys,Language).

/* outputs
?- translatee([deine,schöner,heiligtum],X,german).
X = [?, beautiful, sanctuary] ;
false.

?- translatee([dein,schöner,heiligtum],X,german). 
X = [your, beautiful, sanctuary] ;
false.

?- translatee([feure,trunken],X,german).         
X = [?, drunk] ;
false.

?- translatee([wo,dein,sanfter,flügel,weilt],X,german).
X = [?, your, ?, ?, ?] ;
false.

?- translatee([dissolvit,u,glaciem],X,latin).          
X = [melts, ?, ice] ;
false.

*/