:- module(strings, [
  output/3,
  outputln/3,
  outputf/4,
  outputfln/4
]).

outputf(String, Format, Arguments, Result) :-
  format(string(Formatted), Format, Arguments),
  string_concat(String, Formatted, String).