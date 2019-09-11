:- use_module(math/matrix).
:- use_module(parser/matrices).

start :-
  parse_matrix("{ -5, 4; -1, 0 }", MatrixA),
  parse_matrix("{ 0, 1; 1, 1 }", MatrixB),
  multiply(MatrixA, MatrixB, MatrixC),
  stringify_matrix(MatrixC, S),
  writeln(S). 
