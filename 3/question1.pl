%% Part a

% Dictionary of Latin-English word-pairs in lists nested in a list.
% No punctuation marks.

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

% Function to translate a word between Latin and English using the hand-built dictionary.

translate_word(L,E) :- dictionary_hand(Ds),translate_word(L,Ds,E).  % Translate a Latin and/or English word using the hand-built dictionary.
translate_word(L,[[L,E]|_],E).                                      % Compare the given Latin and/or English word(s) with the first word-pair in the dictionary.
translate_word(L,[_|Ds],E) :- translate_word(L,Ds,E).               % Recursion: if the translation is not in the first word-pair, move onto checking the next word-pair in the dictionary.

%% Part b

% Function to translate a list of words between Latin and English using the hand-built dictionary.

translate_list([],[]).                                                              % Base case: no words of either language.
translate_list(Ls,Es) :- dictionary_hand(Ds),translate_list(Ls,Ds,Es).              % Translate a list of Latin and/or English words using the hand-built dictionary.
translate_list([L|Ls],Ds,[E|Es]) :- translate_word(L,Ds,E),translate_list(Ls,Es).   % Recursion: translate the first given Latin and/or English words(s) using the translate_word function, and recurse with the remaining list of words.

% Querying first 3 lines of the poem.
%
% ?- translate_list([o,fortuna],X).
% X = [o, fortune] ;
% false.
%
% ?- translate_list([velut,luna],X).
% X = [like, 'the moon'] ;
% false.
%
% ?- translate_list([statu,variabilis],X).
% X = [youre, changeable] ;
% false.

% Querying specific phrases.
%
% ?- translate_list([o,luna,statu,velut,fortuna],X).
% X = [o, 'the moon', youre, like, fortune] ;
% false.
%
% ?- translate_list([variabilis,fortuna],X).
% X = [changeable, fortune] ;
% false.
%
% ?- translate_list([statu,velut,luna],X).
% X = [youre, like, 'the moon'] ;
% false.
%
% ?- translate_list([potestatem,nunc,curat,tunc,obdurat],X).
% X = [power, first, soothes, then, oppresses] ;
% false.
%
% ?- translate_list([egestatem,detestabilis],X).
% X = [poverty, hateful] ;
% false.
%
% ?- translate_list([statu,crescis,et,decrescis],X).
% X = [youre, waxing, and, waning] ;
% false.

%% Part c

dictionary_build([],[],[]).
dictionary_build([L1|Ls],[E1|Es],[[L1,E1]|X]) :- dictionary_build(Ls,Es,X).

translate(Ls,D,Es) :- translate_list(Ls,D,Es).

% Word translation lists
%
% o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem
% o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice

% Building the dictionary
%
% dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,fortuna,o,fortuna],D,X).

% Querying first 3 lines of the poem.
%
% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,fortuna,o,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [o, fortune, o, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([velut,luna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [like, 'the moon'] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,variabilis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, changeable] ;
% false.


% Querying specific phrases.
%
% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,luna,statu,velut,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [o, 'the moon', youre, like, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([variabilis,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [changeable, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,velut,luna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, like, 'the moon'] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([potestatem,nunc,curat,tunc,obdurat],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [power, first, soothes, then, oppresses] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([egestatem,detestabilis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [poverty, hateful] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the,moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,crescis,et,decrescis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the,moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, waxing, and, waning] ;
% false.




