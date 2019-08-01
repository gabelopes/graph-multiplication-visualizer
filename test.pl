:- use_module(math/matrix).
:- use_module(parser/matrices).

start :-
  parse_matrix("{ 3, 5, 10, 9, 12; 1, 2, 4, -15, 1; -9, 6, 0, 10, 40; 3, -3, 6, 6, -1; 0, 0, 1, 2, 3 }", Matrix),
  determinant(Matrix, Determinant),
  writeln(Determinant). 
