/* :- initialization(main). */

contains_duplicate([A|R]) :-
  string_chars(A,Ac),
  msort(Ac,Sc),
  member(B,R),
  string_chars(B,Bc),
  msort(Bc,Sc).
contains_duplicate([_|R]) :-
  contains_duplicate(R).

read_lines(Fd,[W|L]) :-
  read_string(Fd, "\n", "", Eof, W),
  !,
  ((Eof is -1, !, L = []); read_lines(Fd,L)).

line_words(L,W) :- split_string(L," ", "", W).

lines_words([],[]).
lines_words([A|R],[B|S]) :-
  line_words(A,B),
  lines_words(R,S).

count_nodups([],0).
count_nodups([A|R],C) :-
  (contains_duplicate(A); A = [""]),
  !,
  count_nodups(R,C).
count_nodups([_|R],C) :-
  count_nodups(R,N),
  ((nonvar(N), C is N + 1); (nonvar(C), N is C - 1)).

main([Filename]) :-
  open(Filename, read, Fd),
  read_lines(Fd,L),
  !,
  lines_words(L,G),
  count_nodups(G,C),
  !,
  writeln(C),
  close(Fd),
  halt.
main(_) :-
  halt(1).