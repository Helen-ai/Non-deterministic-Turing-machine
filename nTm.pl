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
