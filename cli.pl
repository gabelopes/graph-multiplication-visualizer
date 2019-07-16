:- use_module(library(optparse)).

:- use_module(parser/digraphs).
:- use_module(parser/matrices).
:- use_module(matrix).
:- use_module(graphviz).

specification([
  [opt('command'), shortflags(['c']), longflags(['command']), type(atom)],
  [opt('folder'), shortflags(['f']), longflags(['folder']), type(atom)]
]).

process_command(dot, [MatrixDefinition], _) :-
  parse_matrix(MatrixDefinition, Matrix),
  generate_digraph(Matrix, Digraph),
  stringify_digraph(Digraph, DOT),
  writeln(DOT).

process_command(mul, [MatrixDefinitionA, MatrixDefinitionB], _) :-
  parse_matrix(MatrixDefinitionA, MatrixA),
  parse_matrix(MatrixDefinitionB, MatrixB),
  multiply(MatrixA, MatrixB, MatrixAB),
  stringify_matrix(MatrixAB, MatrixDefinitionAB),
  writeln(MatrixDefinitionAB).

process_command(vis, [MatrixDefinitionA, MatrixDefinitionB], Options) :-
  parse_matrix(MatrixDefinitionA, MatrixA),
  parse_matrix(MatrixDefinitionB, MatrixB),
  multiply(MatrixA, MatrixB, MatrixAB),
  generate_digraph(MatrixA, DigraphA),
  generate_digraph(MatrixB, DigraphB),
  generate_digraph(MatrixAB, DigraphAB),
  option(folder(Folder), Options),
  resolve_filename(Folder, 'a.png', ImageA),
  resolve_filename(Folder, 'b.png', ImageB),
  resolve_filename(Folder, 'ab.png', ImageAB),
  create_digraph_image(DigraphA, ImageA),
  create_digraph_image(DigraphB, ImageB),
  create_digraph_image(DigraphAB, ImageAB),
  format("Images created successfully at '~w'.\n", [Folder]).

process_command(visq, [MatrixDefinition], Options) :-
  process_command(visp, [MatrixDefinition, '2'], Options).

process_command(visp, [MatrixDefinition, ExponentAtom], Options) :-
  atom_number(ExponentAtom, Exponent),
  parse_matrix(MatrixDefinition, Matrix),
  power(Matrix, Exponent, PoweredMatrix),
  generate_digraph(Matrix, DigraphA),
  generate_digraph(PoweredMatrix, DigraphP),
  option(folder(Folder), Options),
  resolve_filename(Folder, 'a.png', ImageA),
  resolve_filename(Folder, 'p.png', ImageP),
  create_digraph_image(DigraphA, ImageA),
  create_digraph_image(DigraphP, ImageP),
  format("Images created successfully at '~w'.\n", [Folder]).

process_command(pow, [MatrixDefinition, ExponentAtom], _) :-
  atom_number(ExponentAtom, Exponent),
  parse_matrix(MatrixDefinition, Matrix),
  power(Matrix, Exponent, PoweredMatrix),
  stringify_matrix(PoweredMatrix, PoweredMatrixDefinition),
  writeln(PoweredMatrixDefinition).

resolve_filename(Folder, Filename, Resolved) :-
  atom_concat(Folder, '/', NormaliedFolder),
  atom_concat('./', Filename, NormaliedFilename),
  relative_file_name(Resolved, NormaliedFolder, NormaliedFilename).

start :-
  current_prolog_flag(argv, Arguments),
  specification(Specification),
  opt_parse(Specification, Arguments, Options, Positional, [allow_empty_flag_spec(false)]),
  option(command(Command), Options),
  process_command(Command, Positional, Options),
  halt.
