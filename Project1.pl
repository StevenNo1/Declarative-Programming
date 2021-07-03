%

:- ensure_loaded(library(clpfd)).

/*
This puzzle_solution is the main body of this solution and it conclude
all the term that we would use.
The first maplist is to check all the lines have same length
Then, define Puzzle as Head and Tail
check the diagonal, rows and columns of the puzzle
finaly, print the Puzzle and check whether all the instances are grounded.
*/
puzzle_solution(Puzzle):-
    maplist(same_length(Puzzle),Puzzle),
    Puzzle=[Head|Tail],
    same_diagonal(Tail,1,_),
    check_rows(Tail),
    check_columns(Puzzle),
    maplist(label,Puzzle),
    ground(Puzzle).

/*
check all the instance in the diagonal have same number
*/
same_diagonal([],_,_).
same_diagonal([L|Ls],N,Compare):-
    nth0(N,L,Compare),
    N1 #= N + 1,
    same_diagonal(Ls,N1,Compare).
/*
use add function and "#="(This Declarative integer arithmetic is often used in CLP(FD))
(website:https://www.swi-prolog.org/pldoc/man?section=clpfd-integer-arith)
*/
add(I1,I2,Result):-
    Result #= I1 + I2.
/*
apply sum_List by foldl() predicate
*/
sum_List(List,Result):-
    foldl(add,List,0,Result).
/*
implement multiply predicate. Similar to add function Using #= and foldl
*/
multiply(I1,I2,Result):-
    Result #= I1 * I2.

multiply_list(List,Result):-
    foldl(multiply,List,1,Result).
/*
check_row/1 predicate include Head and Tail
first line is check all the integers are between 1 to 9.
Second line is check all the integers are unique.
Then, in the question illustrate that all the rows and columns are multiply or sum.
Therefore, the next condition would be either sum_List or multiply_list 
*/
check_row([Head|Tail]):-
    Tail ins 1..9,
    all_distinct(Tail),
    (sum_List(Tail,Head)
    ;multiply_list(Tail,Head)
    ).
/*
check_rows function would check the Taill using check_row predicate
*/
check_rows(Tail):-
    maplist(check_row,Tail).
/*
check_columns/1 would transpose the rows, then, use check_rows/1 predicate to
check its correctness.
*/

check_columns(Puzzle):-
    transpose(Puzzle,[Head|Tail]),
    check_rows(Tail).

