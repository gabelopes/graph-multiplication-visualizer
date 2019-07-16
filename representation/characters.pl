:- module(characters, [
  generate_character_sequence/2
]).

generate_character_sequence(Index, Sequence) :-
  Index >= 0,
  Quotient is Index // 26,
  Remainder is Index mod 26,
  generate_quotient_sequence(Quotient, QuotientSequence),
  get_letter(Remainder, RemainderLetter),
  atomic_list_concat([QuotientSequence, RemainderLetter], Sequence).

generate_quotient_sequence(Quotient, Sequence) :-
  Quotient > 26, !,
  OffsetQuotient is Quotient - 1,
  generate_sequence(OffsetQuotient, Sequence).
generate_quotient_sequence(Quotient, Sequence) :-
  Quotient > 0, !,
  OffsetQuotient is Quotient - 1,
  get_letter(OffsetQuotient, Sequence).
generate_quotient_sequence(_, '').

get_letter(Index, Character) :-
  Code is Index + 65,
  char_code(Character, Code).