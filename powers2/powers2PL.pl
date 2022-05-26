find_one(X,Y):-
    Z is log(X)/log(2),
    A is truncate(Z),
    Y is **(2,A).

afairesh(X,Y):-
    find_one(X,Z),
    Y is X-Z.

takeList(0, []).

takeList(A, [H|T]):-
    find_one(A,B),
    H is B,
    afairesh(A,A2),
    takeList(A2, T),
    !.

find_pow(0, Z, Z).

find_pow(A, [1|T], [H2|T2]):-
    H2 is 1,
    find_pow(A, T, T2).

find_pow(A, [H|T], Z):-
    H =\= 1,
    Temp is div(H,2),
    A2 is A-1,
    find_pow(A2, [Temp,Temp|T], Z),
    !.  

sol([X,Y], Z):-
    takeList(X,Li),
    reverse(Li,C),
    length(C,D),
    (X<Y -> Z=[]
    ;D>Y -> Z=[]
    ;A is Y-D,
     find_pow(A, C, Z)
    ).

final([], _, P, [P]).

final([H|T], K, P, [H2|T2]):-
    Pow is **(2, K),
    (H=:=Pow -> P2 is P+1,
                final(T, K, P2, [H2|T2])
    ;           K2 is K+1,
                H2 is P,
                final([H|T], K2, 0, T2)
    ). 

forall(Li, Z):-
    (sol(Li, []),
     Z=[],
     !
    ;sol(Li, Z2),
     final(Z2, 0, 0, Z),
     !
    ).

printAll([], []).

printAll([H|T], [H2|T2]):-
    forall(H, H2),
    printAll(T, T2). 

powers2(File, Answers):-  
    once(read_input(File, _, C)),
    printAll(C, Answers).

read_input(File, N, C) :-
    open(File, read, Stream),
    read_line(Stream, [N]),
    read_T(Stream, N, C).

read_T(_ , 0, []).

read_T(Stream, N, [H|T]) :-
    read_line(Stream, H2),
    H = H2,
    N2 is N-1,
    read_T(Stream, N2, T).
    

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).
        