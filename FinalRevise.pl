
skill(darren,cooking,15).
skill(linda,design,5).
skill(jessie,woodwork,10).
skill(john,painting,8).
skill(jessie,painting,15).
skill(linda,cooking,3).

job(catering, [cooking]).
job(construction, [design,woodwork,painting]).

/*
books([],[]).
books([Author-Title|Books],[Author-N|Records]):-
  ground(Books),
  bagof(T,(Author)^[Author-T|Books],Bag),
  length(Bag,N).


:- ensure_loaded(library(pairs)).
books([], []).
books(Xs, Ys):-
    pairs_keys(Xs, Ks),
    sort(Ks, Uks),
    helper(Uks, Ks, Ys).

helper([], _, []).
helper([U|Us], Ks, Rs):-
    getN(U, Ks, N),
    append([U-N], R1, Rs),
    helper(Us, Ks, R1).


getN(W, [W|Ks], N):- getN(W, Ks, N1), N is N1 + 1.
getN(W, [K|Ks], N):- W\=K, getN(W, Ks, N).
getN(_, [], 0).


books([],[]).
books(Books,[Author-N|Records]):-
  %Books = [Author-Title|Tail],
  ground(Books),
  bagof(T,(Author)^[Author-T|Books],Bag),
  length(Bag,N).


books([],[]).
books(Books,Records):-
   Books = [Author-Title|Books],
   Records = [Author-N|Records],
   bagof(Title, (N,Author)^([Author-Title|Books], [Author-N|Records]), Bag),
   length(Title, N)


books([],[]).
books(Books, Records):-
   books(X,Y,N).

books

books([],[]).
books([Author-Title|Books],[Author-N|Records]):-
  ground(Books),
  bagof(T,(Author)^[Author-T|Books],Bag),
  length(Bag,N),
  books(Books,Records).

%the book(,) function implemented by others

get_authors([],[]).
get_authors([(A-_)|Books], Authors):-
    append([A], As, Authors),
    get_authors(Books,As).

get_records([L],[],R,Count):- R = [L-Count].
get_records([L|Ls], [ML|MLs], R, Count):-
    (
        L\= ML ->
        append([L-Count],Rs,R),
        get_records(Ls, [ML|MLs], Rs, 0)
        ;
        Count1 is Count + 1,
        get_records([L|Ls], MLs, R, Count1)
    ).
books([],[]).
books(Books,Records):-
    get_authors(Books,Authors),
    %sort(Authors,Keys),
    %msort(Authors,Mkeys),
    get_records(Keys,Mkeys,Records,0).

%sum the numbers in list
sum([],0).
sum([L|Ls],N):- sum(Ls,N1),N is N1+L.
*/
