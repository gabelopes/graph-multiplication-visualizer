:- module(arrays, [
  insert_element/4,
  remove_element/3,
  remove_element/4,
  prepend_all/3
]).

is_out_of_insert_bounds(List, Index) :-
  length(List, Length),
  Index =< 0,
  Index > Length + 1.

insert_element(Element, List, Index, Result) :-
  \+ is_out_of_insert_bounds(List, Index),
  insert_element(Element, List, 1, Index, Result).
insert_element(Element, [], Index, Index, [Element]).
insert_element(Element, [CurrentElement|Rest], Index, Index, [Element, CurrentElement|Rest]).
insert_element(Element, [CurrentElement|Rest], CurrentIndex, Index, [CurrentElement|Partial]) :-
  NextIndex is CurrentIndex + 1,
  insert_element(Element, Rest, NextIndex, Index, Partial).

is_out_of_remove_bounds(List, Index) :-
  length(List, Length),
  Index =< 0,
  Index > Length. 

remove_element(List, Index, Result) :-
  remove_element(List, Index, Result, _).
remove_element(List, Index, Result, Element) :-
  \+ is_out_of_remove_bounds(List, Index),
  remove_element(List, 1, Index, Result, Element).

remove_element([RemovedElement|Rest], ElementIndex, ElementIndex, Rest, RemovedElement).
remove_element([Element|Rest], Index, ElementIndex, [Element|Partial], RemovedElement) :-
  NextIndex is Index + 1,
  remove_element(Rest, NextIndex, ElementIndex, Partial, RemovedElement).

prepend_all(_, [], []).
prepend_all(Element, [List|Rest], [[Element|List]|Partial]) :-
  prepend_all(Element, Rest, Partial).