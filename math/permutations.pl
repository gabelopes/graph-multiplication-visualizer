:- module(permutations, [
  permute/2,
  signed_permute/2
]).

:- use_module('../representation/arrays').

permute([], _) :- fail.
permute([_], _) :- permute([], _).
permute([A, B], [[A, B], [B, A]]).
permute(Set, Permutations) :-
  permute(Set, 1, Permutations).
permute(Set, Index, []) :-
  length(Set, Length),
  Index > Length.
permute(Set, Index, Permutations) :-
  % Recursion
  NextIndex is Index + 1,
  permute(Set, NextIndex, PartialPermutations),
  % Central Algorithm
  remove_element(Set, Index, Minor, Element),
  permute(Minor, MinorPermutations),
  prepend_all(Element, MinorPermutations, CurrentPermutations),
  % Joining
  append(CurrentPermutations, PartialPermutations, Permutations).

signed_permute(Set, SignedPermutations) :-
  permute(Set, UnsignedPermutations),
  sign_permutations(UnsignedPermutations, SignedPermutations).

sign_permutations(Permutations, Signed) :-
  sign_permutations(Permutations, 1, Signed).
sign_permutations([], _, []).
sign_permutations([Permutation|Rest], Index, [permutation(Permutation, Sign)|Partial]) :-
  sign_permutation(Index, Sign),
  NextIndex is Index + 1,
  sign_permutations(Rest, NextIndex, Partial).

sign_permutation(Index, Sign) :-
  Parameter is Index * pi / 2,
  Sign is round(cos(Parameter) + sin(Parameter)).