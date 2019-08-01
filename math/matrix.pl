:- module(matrix, [
  multiply/3,
  power/3,
  identity/2,
  null/2,
  null/3,
  collect_elements/2,
  generate_matrix/3,
  generate_matrix/4,
  determinant/2
]).

:- use_module(library(clpfd)).
:- use_module('../representation/arrays').
:- use_module(sets).
:- use_module(permutations).

%%% Mathematical Operations
% Multiplication
multiply(matrix(RowsA, ColumnsA, MatrixA), matrix(ColumnsA, ColumnsB, MatrixB), matrix(RowsA, ColumnsB, MatrixC)) :-
  transpose(MatrixB, TransposedB),
  multiply(MatrixA, TransposedB, MatrixC).

multiply([], _, []).
multiply([Row|RowsRest], Columns, [Elements|ElementsRest]) :-
  multiply_scalars(Row, Columns, Elements),
  multiply(RowsRest, Columns, ElementsRest).

multiply_scalars(_, [], []).
multiply_scalars(Row, [Column|ColumnRest], [Element|ElementRest]) :-
  multiply_scalar(Row, Column, Element),
  multiply_scalars(Row, ColumnRest, ElementRest).

multiply_scalar([], [], 0).
multiply_scalar([RowElement|RowRest], [ColumnElement|ColumnRest], Scalar) :-
  multiply_scalar(RowRest, ColumnRest, PartialScalar),
  Scalar is RowElement * ColumnElement + PartialScalar.

% Power
power(matrix(Size, Size, _), 0, Result) :-
  identity(Size, Result).
power(matrix(Size, Size, Matrix), 1, matrix(Size, Size, Matrix)).
power(matrix(Size, Size, Matrix), Exponent, Result) :-
  PreviousExponent is Exponent - 1,
  power(matrix(Size, Size, Matrix), PreviousExponent, Partial),
  multiply(matrix(Size, Size, Matrix), Partial, Result).
%%% Element Operations
% Identity Matrix
map_identity(Index, Index, 1).
map_identity(_, _, 0).

identity(1, [1]).
identity(Size, Matrix) :-
  Size > 1,
  generate_matrix(Size, Size, map_identity, Matrix).

% Null Matrix
map_null(_, _, 0).

null(Size, Matrix) :-
  null(Size, Size, Matrix).
null(Rows, Columns, Matrix) :-
  Rows > 0,
  Columns > 0,
  generate_matrix(Rows, Columns, map_null, Matrix).

% Representation
collect_elements(matrix(_, _, Matrix), List) :-
  collect_rows(Matrix, 1, ElementsRows),
  append(ElementsRows, List).

collect_rows([], _, []).
collect_rows([Row|Rest], RowIndex, [Elements|Partial]) :-
  collect_columns(Row, RowIndex, 1, Elements),
  NextRowIndex is RowIndex + 1,
  collect_rows(Rest, NextRowIndex, Partial).

collect_columns([], _, _, []).
collect_columns([Element|Rest], RowIndex, ColumnIndex, [element(RowIndex, ColumnIndex, Element)|Partial]) :-
  NextColumnIndex is ColumnIndex + 1,
  collect_columns(Rest, RowIndex, NextColumnIndex, Partial).

% Generation
generate_matrix(Size, Mapper, Result) :-
  generate_matrix(Size, Size, Mapper, Result).

generate_matrix(Rows, Columns, Mapper, matrix(Rows, Columns, Matrix)) :-
  Rows >= 0,
  Columns >= 0,
  generate_rows(Rows, Columns, 0, Mapper, Matrix).

generate_rows(Rows, _, Rows, _, []).
generate_rows(Rows, Columns, RowIndex, Mapper, [Column|Rest]) :-
  generate_columns(Columns, RowIndex, 0, Mapper, Column),
  NextRowIndex is RowIndex + 1,
  generate_rows(Rows, Columns, NextRowIndex, Mapper, Rest).

generate_columns(Columns, _, Columns, _, []).
generate_columns(Columns, RowIndex, ColumnIndex, Mapper, [Element|Rest]) :-
  apply(Mapper, [RowIndex, ColumnIndex, Element]),
  NextColumnIndex is ColumnIndex + 1,
  generate_columns(Columns, RowIndex, NextColumnIndex, Mapper, Rest).

%% Determinant
determinant(Matrix, Determinant) :-
  leibniz_determinant(Matrix, Determinant).

leibniz_determinant(matrix(Size, Size, Matrix), Determinant) :-
  range(1, Size, Set),
  signed_permute(Set, Permutations),
  collect_elements(matrix(Size, Size, Matrix), Elements),
  leibniz_determinant(Permutations, Elements, Determinant).

leibniz_determinant([], _, 0).
leibniz_determinant([permutation(Permutation, Sign)|Rest], Elements, Determinant) :-
  leibniz_determinant(Rest, Elements, PartialDeterminant),
  leibniz_product(Elements, Permutation, Product),
  Determinant is Sign * Product + PartialDeterminant.

leibniz_product(Elements, Permutation, Product) :-
  leibniz_product(Elements, 1, Permutation, Product).
leibniz_product(_, _, [], 1).
leibniz_product(Elements, Row, [Column|Rest], Product) :-
  NextRow is Row + 1,
  leibniz_product(Elements, NextRow, Rest, PartialProduct),
  element_at(Elements, Row, Column, Element),
  Product is Element * PartialProduct.

element_at([], _, _, _) :- fail.
element_at([element(Row, Column, Element)|_], Row, Column, Element).
element_at([_|Rest], Row, Column, Element) :-
  element_at(Rest, Row, Column, Element).