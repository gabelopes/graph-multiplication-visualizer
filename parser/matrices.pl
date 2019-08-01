:- module(matrices, [
  parse_matrix/2,
  stringify_matrix/2
]).

:- use_module(commons).

matrix(Rows, Columns, Matrix) -->
  spaces,
  left_brace,
  spaces,
  declaration(Columns, Matrix),
  { length(Matrix, Rows) },
  spaces,
  right_brace,
  spaces.

declaration(0, []) --> [].
declaration(Columns, Matrix) --> rows(Columns, Matrix).

rows(Columns, [Row]) -->
  row(Row),
  { length(Row, Columns) }.
rows(Columns, [Row|Rest]) -->
  row(Row),
  { length(Row, Columns) },
  spaces,
  semicolon,
  spaces,
  rows(Columns, Rest).

row([Number]) --> integer(Number).
row([Number|Rest]) -->
  integer(Number),
  spaces,
  comma,
  spaces,
  row(Rest).

% Parsing
parse_matrix(String, matrix(Rows, Columns, Matrix)) :-
  string_codes(String, Stream),
  phrase(matrix(Rows, Columns, Matrix), Stream).

stringify_matrix(matrix(Rows, Columns, Matrix), String) :-
  phrase(matrix(Rows, Columns, Matrix), Stream),
  string_codes(String, Stream).
