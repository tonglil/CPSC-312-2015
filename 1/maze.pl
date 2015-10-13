connects_to(r11,r12).
connects_to(r12,r13).
connects_to(r13,r14).
connects_to(r14,r15).
connects_to(r15,r16).
connects_to(r16,r17).
connects_to(r11,r21).
connects_to(r12,r22).
connects_to(r14,r24).
connects_to(r24,r23).
connects_to(r25,r26).
connects_to(r27,r28).
connects_to(r17,r27).
connects_to(r28,r18).
connects_to(r21,r31).
connects_to(r22,r32).
connects_to(r33,r34).
connects_to(r34,r35).
connects_to(r35,r25).
connects_to(r26,r36).
connects_to(r38,r37).
connects_to(r31,r41).
connects_to(r32,r42).
connects_to(r43,r33).
connects_to(r36,r46).
connects_to(r37,r47).
connects_to(r48,r38).
connects_to(r41,r51).
connects_to(r42,r52).
connects_to(r52,r53).
connects_to(r53,r43).
connects_to(r44,r54).
connects_to(r45,r44).
connects_to(r46,r45).
connects_to(r58,r48).
connects_to(r51,r61).
connects_to(r62,r63).
connects_to(r54,r64).
connects_to(r64,r65).
connects_to(r65,r55).
connects_to(r46,r56).
connects_to(r56,r66).
/* below is the one to comment out for Part B */
connects_to(r66,r67).
connects_to(r67,r57).
connects_to(r57,r58).
connects_to(r58,r68).
connects_to(r61,r71).
connects_to(r72,r62).
connects_to(r63,r73).
connects_to(r71,r81).
connects_to(r81,r82).
connects_to(r82,r72).
connects_to(r73,r83).
connects_to(r83,r84).
connects_to(r84,r74).
connects_to(r74,r75).
connects_to(r75,r76).
connects_to(r75,r85).
connects_to(r85,r86).
connects_to(r68,r78).
connects_to(r78,r77).
connects_to(r77,r87).
connects_to(r87,r88).

/* Here are twelve different ways to construct the path procedure */

path01(X,Y) :- connects_to(X,Y).
path01(X,Y) :- connects_to(X,Z),path01(Z,Y).

path02(X,Y) :- connects_to(X,Z),path02(Z,Y).
path02(X,Y) :- connects_to(X,Y).

% ok
% overflow
path03(X,Y) :- connects_to(X,Y).
path03(X,Y) :- path03(Z,Y),connects_to(X,Z).

% overflow 1
% overflow
path04(X,Y) :- path04(Z,Y),connects_to(X,Z).
path04(X,Y) :- connects_to(X,Y).

% ok
% overflow
path05(X,Y) :- connects_to(X,Y).
path05(X,Y) :- path05(X,Z),connects_to(Z,Y).

% overflow 1
% overflow
path06(X,Y) :- path06(X,Z),connects_to(Z,Y).
path06(X,Y) :- connects_to(X,Y).

path07(X,Y) :- connects_to(X,Y).
path07(X,Y) :- connects_to(Z,Y),path07(X,Z).

path08(X,Y) :- connects_to(Z,Y),path08(X,Z).
path08(X,Y) :- connects_to(X,Y).

% overflow 1
% overflow
path09(X,Y) :- connects_to(X,Y).
path09(X,Y) :- path09(X,Z),path09(Z,Y).

% overflow 1
% overflow
path10(X,Y) :- path10(X,Z),path10(Z,Y).
path10(X,Y) :- connects_to(X,Y).

% ok
% overflow
path11(X,Y) :- connects_to(X,Y).
path11(X,Y) :- path11(Z,Y),path11(X,Z).

% overflow 1
% overflow
path12(X,Y) :- path12(Z,Y),path12(X,Z).
path12(X,Y) :- connects_to(X,Y).
