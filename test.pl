:- use_module(math/permutations).
:- use_module(math/sets).

start :-
  signed_permute([1, 2, 3, 4], Permutations),
  length(Permutations, Length),
  range(1, 1000, Set),
  writeln(Set),
  format("|P| = ~w\n~w\n", [Length, Permutations]). 
