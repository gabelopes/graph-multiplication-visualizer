:- module(commons, [
  number/3,
  digit/3,
  character_sequence/3,
  character/3,
  spaces/2,
  required_spaces/2,
  new_line/2,
  left_brace/2,
  right_brace/2,
  left_bracket/2,
  right_bracket/2,
  comma/2,
  semicolon/2,
  equals/2
]).

% Integers
number(Number) -->
  { var(Number) },
  digits(Digits),
  { number_chars(Number, Digits) }.
number(Number) -->
  {
    nonvar(Number),
    number_chars(Number, Digits)
  },
  digits(Digits).

digits([Digit]) --> digit(Digit).
digits([Digit|Rest]) -->
  digit(Digit),
  digits(Rest).

digit('0') --> "0".
digit('1') --> "1".
digit('2') --> "2".
digit('3') --> "3".
digit('4') --> "4".
digit('5') --> "5".
digit('6') --> "6".
digit('7') --> "7".
digit('8') --> "8".
digit('9') --> "9".

% Character Sequences
character_sequence(Sequence) -->
  { var(Sequence) },
  characters(Characters),
  { atom_chars(Sequence, Characters) }.
character_sequence(Sequence) -->
  {
    nonvar(Sequence),
    atom_chars(Sequence, Characters)
  },
  characters(Characters).

characters([Character]) --> character(Character).
characters([Character|Rest]) -->
  character(Character),
  characters(Rest).

character('A') --> "A".
character('B') --> "B".
character('C') --> "C".
character('D') --> "D".
character('E') --> "E".
character('F') --> "F".
character('G') --> "G".
character('H') --> "H".
character('I') --> "I".
character('J') --> "J".
character('K') --> "K".
character('L') --> "L".
character('M') --> "M".
character('N') --> "N".
character('O') --> "O".
character('P') --> "P".
character('Q') --> "Q".
character('R') --> "R".
character('S') --> "S".
character('T') --> "T".
character('U') --> "U".
character('V') --> "V".
character('W') --> "W".
character('X') --> "X".
character('Y') --> "Y".
character('Z') --> "Z".

% Other
spaces --> [].
spaces --> space, spaces.

required_spaces --> space, spaces.

space --> " ".
space --> "\s".
space --> "\t".
space --> "\v".
space --> new_line.

new_line --> "\n\r".
new_line --> "\r\n".
new_line --> "\n".
new_line --> "\r".

left_brace --> "{".
right_brace --> "}".
left_bracket --> "[".
right_bracket --> "]".
comma --> ",".
semicolon --> ";".
equals --> "=".