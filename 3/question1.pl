%% Part a

% Dictionary of Latin-English word-pairs in lists nested in a list.
% No punctuation marks.

dictionary_latin([
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

% Function to translate a word between Latin and English using the hand-built Latin dictionary.

translate_word_latin(L,E) :- dictionary_latin(Ds),translate_word(L,Ds,E).   % Translate a Latin and/or English word using the hand-built Latin dictionary.

translate_word(W,[[W,E]|_],E).                                      % Compare the given foreign and/or English word(s) with the first word-pair in the dictionary.
translate_word(W,[_|Ds],E) :- translate_word(W,Ds,E).               % Recursion: if the translation is not found in the first word-pair, check the next word-pair in the dictionary.

%% Part b

% Function to translate a list of words between Latin and English using the hand-built Latin dictionary.

translate_latin([],[]).                                             % Base case: no words of either language.
translate_latin(Ls,Es) :- dictionary_latin(Ds),translate(Ls,Ds,Es). % Translate a list of Latin and/or English words using the hand-built Latin dictionary.

translate([],_,[]).                                                         % Base case: no words of either language.
translate([W|Ws],Ds,[E|Es]) :- translate_word(W,Ds,E),translate(Ws,Ds,Es).  % Recursion: translate the first given foreign and/or English words(s) with a given dictionary using the translate_word function, and recurse with the remaining list of words and the same dictionary.

% Querying first 3 lines of the poem.
%
% ?- translate_latin([o,fortuna],X).
% X = [o, fortune] ;
% false.
%
% ?- translate_latin([velut,luna],X).
% X = [like, 'the moon'] ;
% false.
%
% ?- translate_latin([statu,variabilis],X).
% X = [youre, changeable] ;
% false.

% Querying specific phrases.
%
% ?- translate_latin([o,luna,statu,velut,fortuna],X).
% X = [o, 'the moon', youre, like, fortune] ;
% false.
%
% ?- translate_latin([variabilis,fortuna],X).
% X = [changeable, fortune] ;
% false.
%
% ?- translate_latin([statu,velut,luna],X).
% X = [youre, like, 'the moon'] ;
% false.
%
% ?- translate_latin([potestatem,nunc,curat,tunc,obdurat],X).
% X = [power, first, soothes, then, oppresses] ;
% false.
%
% ?- translate_latin([egestatem,detestabilis],X).
% X = [poverty, hateful] ;
% false.
%
% ?- translate_latin([statu,crescis,et,decrescis],X).
% X = [youre, waxing, and, waning] ;
% false.

%% Part c

dictionary_build([],[],[]).
dictionary_build([W1|Ws],[E1|Es],[[W1,E1]|D]) :- dictionary_build(Ws,Es,D).

% Word translation lists
%
% o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem
% o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice

% Building the dictionary
%
% dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,fortuna,o,fortuna],D,X).

% Querying first 3 lines of the poem.
%
% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [o, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([velut,luna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [like, 'the moon'] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,variabilis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, changeable] ;
% false.

% Querying specific phrases.
%
% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([o,luna,statu,velut,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [o, 'the moon', youre, like, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([variabilis,fortuna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [changeable, fortune] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,velut,luna],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, like, 'the moon'] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([potestatem,nunc,curat,tunc,obdurat],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [power, first, soothes, then, oppresses] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([egestatem,detestabilis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [poverty, hateful] ;
% false.

% ?- dictionary_build([o,fortuna,velut,luna,statu,variabilis,semper,crescis,aut,decrescis,vita,detestabilis,nunc,obdurat,et,tunc,curat,egestatem,potestatem,dissolvit,ut,glaciem],[o,fortune,like,'the moon',youre,changeable,ever,waxing,and,waning,life,hateful,first,oppresses,and,then,soothes,poverty,power,melts,like,ice],D),translate([statu,crescis,et,decrescis],D,X).
% D = [[o, o], [fortuna, fortune], [velut, like], [luna, 'the moon'], [statu, youre], [variabilis, changeable], [semper, ever], [crescis|...], [...|...]|...],
% X = [youre, waxing, and, waning] ;
% false.

%% Part d

% Dictionary of German-English word-pairs in lists nested in a list.
% No punctuation marks.

dictionary_german([
        [freude,joy],
        [schoner,beautiful],
        [gotter,'of gods'],
        [funken,spark],
        [tochter,daughter],
        [aus,from],
        [elysium,elysium],
        [wir,we],
        [betreten,enter],
        [feuer,'with fire'],
        [trunken,drunk],
        [himmlische,'heavenly being'],
        [dein,your],
        [heiligtum,sanctuary]
    ]).

translate_word_from(Lang,L,E) :- Lang=latin,dictionary_latin(Ds),translate_word(L,Ds,E).    % Translate a Latin and/or English word using the hand-built Latin dictionary.
translate_word_from(Lang,G,E) :- Lang=german,dictionary_german(Ds),translate_word(G,Ds,E).  % Translate a German and/or English word using the hand-built German dictionary.

translate_from(_,[],[]).                                                                % Base case: no words of either language.
translate_from(Lang,Ls,Es) :- Lang=latin,dictionary_latin(Ds),translate(Ls,Ds,Es).      % Translate a list of Latin and/or English words using the hand-built Latin dictionary.
translate_from(Lang,Gs,Es) :- Lang=german,dictionary_german(Ds),translate(Gs,Ds,Es).    % Translate a list of German and/or English words using the hand-built German dictionary.

% Querying first 4 lines of the German poem.
%
% ?- translate_from(german,[freude,schoner,gotter,funken],X).
% X = [joy, beautiful, 'of gods', spark] ;
% false.
%
% ?- translate_from(german,[tochter,aus,elysium],X).
% X = [daughter, from, elysium] ;
% false.
%
% ?- translate_from(german,[wir,betreten,feuer,trunken],X).
% X = [we, enter, 'with fire', drunk] ;
% false.
%
% ?- translate_from(german,[himmlische,dein,heiligtum],X).
% X = ['heavenly being', your, sanctuary] ;
% false.

% Querying the second line of the Latin poem.
%
% ?- translate_from(latin,[velut,luna],X).
% X = [like, 'the moon'] ;
% false.

% Querying specific sentences without source language.
%
% ?- translate_from(Y,[dein,schoner,heiligtum],X).
% Y = german,
% X = [your, beautiful, sanctuary] ;
% false.
%
% ?- translate_from(Y,[feuer,gotter,aus,elysium],X).
% Y = german,
% X = ['with fire', 'of gods', from, elysium] ;
% false.
%
% ?- translate_from(Y,[dein,funken],X).
% Y = german,
% X = [your, spark] ;
% false.
%
% ?- translate_from(Y,[trunken,himmlische],X).
% Y = german,
% X = [drunk, 'heavenly being'] ;
% false.

%% Part e
translate_word_from_failable(W,[[W,E]|_],E).                                                    % Compare the given foreign and/or English word(s) with the first word-pair in the dictionary.
translate_word_from_failable(W,[[W1,_]|Ds],E) :- W\=W1,translate_word_from_failable(W,Ds,E).    % Recursion: if the foreign word translation is not found in the first word-pair, check the next word-pair in the dictionary.
translate_word_from_failable(W,[[_,E1]|Ds],E) :- E\=E1,translate_word_from_failable(W,Ds,E).    % Recursion: if the English word translation is not found in the first word-pair, check the next word-pair in the dictionary.
translate_word_from_failable(_,[],?).                                                           % If the dictionary for foreign words has been exhausted, use ? as the translation.
translate_word_from_failable(?,[],_).                                                           % If the dictionary for English words has been exhausted, use ? as the translation.

start_translate_word_from_failable(Lang,L,E) :- Lang=latin,dictionary_latin(Ds),translate_word_from_failable(L,Ds,E).   % Translate a Latin and/or English word using the hand-built Latin dictionary.
start_translate_word_from_failable(Lang,G,E) :- Lang=german,dictionary_german(Ds),translate_word_from_failable(G,Ds,E). % Translate a German and/or English word using the hand-built German dictionary.

translate_from_failable(_,[],[]).                                                                                                   % Base case: no words of either language.
translate_from_failable(Lang,[W|Ws],[E|Es]) :- start_translate_word_from_failable(Lang,W,E),translate_from_failable(Lang,Ws,Es).    % Recursion: translate the first given foreign and/or English words(s) using the start_translate_word_from_failable function, and recurse with the remaining list of words and the same dictionary.

% Querying specific sentences with unidentified translations.
%
% ?- translate_from_failable(german,[deine,schoner,heiligtum],X).
% X = [?, beautiful, sanctuary] ;
% false.
%
% ?- translate_from_failable(german,[feure,trunken],X).
% X = [?, drunk] ;
% false.
%
% ?- translate_from_failable(german,[wo,dein,sanfter,flugel,weilt],X).
% X = [?, your, ?, ?, ?] ;
% false.
%
% ?- translate_from_failable(latin,[dissolvit,u,glaciem],X).
% X = [melts, ?, ice] ;
% false.
