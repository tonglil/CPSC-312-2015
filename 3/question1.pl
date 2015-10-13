% Question 1

%% Part a

%%% Dictionary of Latin-English word-pairs in lists nested in a list.
%%% No punctuation marks.

dictionary_hand([
        [o,o],
        [fortuna,fortune],
        [velut,like],
        [luna,'the moon'],
        [statu,youre],
        [variabilis,changeable],
        [semper,ever],
        [crescis,waxing],
        [aut,and],
        [decrescis,waning],
        [vita,life],
        [detestabilis,hateful],
        [nunc,first],
        [obdurat,oppresses],
        [et,and],
        [tunc,then],
        [curat,soothes],
        [egestatem,poverty],
        [potestatem,power],
        [dissolvit,melts],
        [ut,like],
        [glaciem,ice]
    ]).

%%% Function to translate a Latin word to English, and vice versa, using the hand-built dictionary.

translate_a(L,E) :- dictionary_hand(Ds),translate_a(L,Ds,E).    % Translate the list of Latin and/or English word using the hand-built dictionary.
translate_a(L,[[L1,E1]|_],E) :- L=L1,E=E1.                      % Use the first word-pair in the dictionary to compare with the given Latin and/or English word(s).
translate_a(L,[_|Ds],E) :- translate_a(L,Ds,E).                 % Recursion: if the translation is not in the first word-pair, move onto checking the next word-pair.

%% Part b

%%% Function to translate a list of Latin words to English, and vice versa, using the hand-built dictionary.

translate_b([],[]).                                                     % The base case.
translate_b(Ls,Es) :- dictionary_hand(Ds),translate_b(Ls,Ds,Es).        % Translate the list of Latin and/or English word using the hand-built dictionary.
translate_b([L|Ls],[[L1,E1]|_],[E|Es]) :- L=L1,E=E1,translate_b(Ls,Es). % Recursion: use the first word-pair in the dictionary to compare with the first given Latin and/or English words(s).
translate_b(Ls,[_|Ds],Es) :- translate_b(Ls,Ds,Es).                     % Recursion: if the translation is not in the first word-pair.

%%% Querying first 3 lines separately (line-by-line).
%%% ?- translate_b([o,fortuna],X).
%%% X = [o, fortune] ;
%%% false.
%%%
%%% ?- translate_b([velut,luna],X).
%%% X = [like, 'the moon'] ;
%%% false.
%%%
%%% ?- translate_b([statu,variabilis],X).
%%% X = [youre, changeable] ;
%%% false.

%%% Querying with phrases.
%%% ?- translate_b([o,luna,statu,velut,fortuna],X).
%%% X = [o, 'the moon', youre, like, fortune] ;
%%% false.
%%%
%%% ?- translate_b([variabilis,fortuna],X).
%%% X = [changeable, fortune] ;
%%% false.
%%%
%%% ?- translate_b([statu,velut,luna],X).
%%% X = [youre, like, 'the moon'] ;
%%% false.
%%%
%%% ?- translate_b([potestatem,nunc,curat,tunc,obdurat],X).
%%% X = [power, first, soothes, then, oppresses] ;
%%% false.
%%%
%%% ?- translate_b([egestatem,detestabilis],X).
%%% X = [poverty, hateful] ;
%%% false.
%%%
%%% ?- translate_b([statu,crescis,et,decrescis],X).
%%% X = [youre, waxing, and, waning] ;
%%% false.

%% Part c

dictionary_build([],[],[]).
dictionary_build([L1|Ls],[E1|Es],[[L1,E1]|X]) :- dictionary_build(Ls,Es,X).

translate([],_,[]).
translate([L1|Ls],D,[E1|Es]) :- same_word(L1,D,E1),translate(Ls,D,Es).

same_word(L,[[L,E]|_],E).
same_word(L,[[L1,_]|Ds],E) :- L\=L1,same_word(L,Ds,E).

%%% Word lists
%%% o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice
%%% o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem

%%% dictionary_build([o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],[o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],D).
%%% dictionary_build([o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],[o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],D),translate_b([o,fortuna,o,fortuna],D,X).

