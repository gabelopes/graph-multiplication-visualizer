:- module(sets, [
  range/3
]).

range(To, To, [To]).
range(From, To, [From|Rest]) :-
  From < To,
  Next is From + 1,
  range(Next, To, Rest).