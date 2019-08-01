:- module(digraphs, [
  parse_digraph/2,
  stringify_digraph/2
]).

:- use_module(commons).

digraph(Edges) -->
  spaces,
  digraph,
  spaces,
  left_brace,
  spaces,
  edges(Edges),
  spaces,
  right_brace,
  spaces.

edges([Edge]) --> edge(Edge).
edges([Edge|Rest]) -->
  edge(Edge),
  required_spaces,
  edges(Rest).

edge(edge(Head, Tail, Weight)) -->
  character_sequence(Head),
  spaces,
  arrow,
  spaces,
  character_sequence(Tail),
  spaces,
  label(Weight).

label(1) --> [].
label(Weight) -->
  left_bracket,
  spaces,
  label,
  spaces,
  equals,
  spaces,
  integer(Weight),
  spaces,
  right_bracket.

digraph --> "digraph".
label --> "label".
arrow --> "->".

% Parsing
parse_digraph(String, Digraph) :-
  string_codes(String, Stream),
  phrase(digraph(Digraph), Stream).

stringify_digraph(Digraph, String) :-
  phrase(digraph(Digraph), Stream),
  string_codes(String, Stream).