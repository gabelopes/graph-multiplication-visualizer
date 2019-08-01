:- module(graphviz, [
  generate_digraph/2,
  create_digraph_image/2,
  create_digraph_image/3
]).

:- use_module('../math/matrix', [collect_elements/2]).
:- use_module(characters, [generate_character_sequence/2]).
:- use_module('../parser/digraphs', [stringify_digraph/2]).

generate_edge(Row, Column, Weight, edge(Head, Tail, Weight)) :-
  generate_character_sequence(Row, Head),
  generate_character_sequence(Column, Tail).

generate_edges([], []).
generate_edges([element(_, _, 0)|ElementsRest], Edges) :-
  generate_edges(ElementsRest, Edges).
generate_edges([element(Row, Column, Weight)|ElementsRest], [Edge|EdgesRest]) :-
  generate_edge(Row, Column, Weight, Edge),
  generate_edges(ElementsRest, EdgesRest).

generate_digraph(Matrix, Digraph) :-
  collect_elements(Matrix, Elements),
  generate_edges(Elements, Digraph).

% GraphViz Integration
invoke_dot(DOT, Format, Filename) :-
  format(atom(Type), '-T~w', [Format]),
  process_create(path(dot), [Type, '-o', Filename], [
    stdin(pipe(Input)),
    process(PID)
  ]),
  write(Input, DOT),
  close(Input),
  process_wait(PID, exit(0)).
invoke_dot(DOT, Format, Filename) :-
  format("Error creating digraph image for '~w' at '~w' with format '~w'.\n", [DOT, Filename, Format]).

create_digraph_image(Digraph, Filename) :-
  create_digraph_image(Digraph, "png", Filename).
create_digraph_image(Digraph, Format, Filename) :-
  stringify_digraph(Digraph, DOT),
  invoke_dot(DOT, Format, Filename).
