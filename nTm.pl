% Homework 1 Solution by Helen Husca

%node=[LeftTapeReverse,RightTape,CurrentState]

nTm(MR,ML,WL,HL,In,Out) :- 
    search([[[],In,q0]],[L,R],MR,ML,WL,HL),
    reverse(L,Lrev),
    append(Lrev,R,Found),
    extract(Found,Out).

%search (BFS) through possible nodes
search([[L,R,Q]|_],[L,R],_,_,_,HL) :- 
    goal(Q,R,HL).

search([[L,R,Q]|Rest],[Ln,Rn],MR,ML,WL,HL) :-
    findall([Ln,Rn,Qn],arc([L,R,Q],[Ln,Rn,Qn],MR,ML,WL),Children),
    append(Rest,Children,NewF),
    search(NewF,[Ln,Rn],MR,ML,WL,HL).

%check if current pair is in halting list
goal(Q,[X|_],HL) :- 
    member([Q,X],HL).

%carry out action
arc([L,R,Q],[Ln,Rn,Qn],MR,ML,WL) :-
    arcCommand(Command,[L,R,Q],[Ln,Rn,Qn],MR,ML,WL,Y),
    carryCommand(Command,[L,R],[Ln,Rn],Y).

%check which command can be carried out at current node
arcCommand(wl,[_,[X|_],Q],[_,_,Qn],_,_,WL,Y) :- 
    member([Q,X,Y,Qn],WL).
arcCommand(ml,[_,[X|_],Q],[_,_,Qn],_,ML,_,_) :- 
    member([Q,X,Qn],ML). 
arcCommand(mr,[_,[X|_],Q],[_,_,Qn],MR,_,_,_) :- 
    member([Q,X,Qn],MR).

%carry out command
carryCommand(wl,[L,[_|T]],[L,[Y|T]],Y).
carryCommand(ml,[[],R],[[],[b-k|R]],_).
carryCommand(ml,[[Y|Ln],[X|T]],[Ln,[Y,X|T]],Y).
carryCommand(mr,[L,[X|[]]],[[X|L],[b-k]], _).
carryCommand(mr,[L,[X|T]],[[X|L],T], _).

%delete blanks at beginning and end of output string
extract(Found,Out) :-
    deleteFirstBKs(Found,NFound),
    deleteLastBKs(NFound,Out),
    !.
    
deleteFirstBKs([b-k|Rest],Found) :-
    deleteFirstBKs(Rest, Found).
deleteFirstBKs(Found, Found).

deleteLastBKs(Found,Out) :-
    reverse(Found,Frev),
    deleteFirstBKs(Frev,Orev),
    reverse(Orev,Out).

%for sample run of language accepted by CFG S -> ε | aSb
ab(Input) :- mr(MR), ml(ML), wl(WL), hl(HL), nTm(MR, ML, WL, HL, Input, _).

mr([[q1,b-k,q2],[q2,a,q2],[q2,b,q2],[q5,b-k,q0]]).
ml([[q2,b-k,q3],[q4,b-k,q5],[q5,a,q5],[q5,b,q5]]).
wl([[q0,a,b-k,q1],[q3,b,b-k,q4]]).
hl([[q0,b-k]]).


/** <examples>
?- nTm([],[],[],[[q0,X]],[b-k,i,n,b-k,p,u,t,b-k,b-k],Out).

?- nTm([[q1,1,q2],[q1,0,q2],[q1,b-k,q2]],[],
   [[q0,0,1,q1], [q2,0,b-k,q1]],
   [[q1,b-k]],
   [0,0,0,0,0],
   Out).

?- nTm([[mr1,h,we],[mr1,e,wl],[mr1,l,wp],[mr1,p,hbk],
   [mr,l,wo],[mr,o,wo],[mp,o,wp]],
   [[q0,0,lbk],[lbk,b-k,lbk]],
   [[q0,0,h,mr1],[we,1,e,mr1],[wl,0,l,mr1],[wp,1,p,mr1],
   [q0,0,l,mr],[wo,1,o,mr],[wo,0,o,mp]],
   [[hbk,b-k]],
   [0,1,0,1],
   Output).  

?- ab([a,a,a,b,b,b]).
?- ab([a,b,b]).

*/

