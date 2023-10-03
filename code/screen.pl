/*********************************************************/
/*                                                       */
/* SCREEN module for the BASIC language                  */
/*   Developed for SWI-Prolog 8.x and above              */
/*                                                       */
/*                                                       */
/*                             Fernando Saenz-Perez 2023 */
/*                                                       */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                                       */
/*                                                       */
/* This is free software: you can redistribute it and/or */
/* modify it under the terms of the GNU Lesser General   */
/* Public License as published by the Free Software      */
/* Foundation, either version 3 of the License, or (at   */
/* your option) any later version.                       */
/*                                                       */
/* This software is distributed in the hope that it will */
/* be useful, but WITHOUT ANY WARRANTY; without even the */
/* implied warranty of MERCHANTABILITY or FITNESS FOR A  */
/* PARTICULAR PURPOSE. See the GNU Lesser General Public */
/* License for more details.                             */
/*                                                       */
/* You should have received a copy of the GNU Lesser     */
/* General Public License and GNU General Public License */
/* along with this program. If not, see:                 */
/*                                                       */
/*            http://www.gnu.org/licenses/               */
/*********************************************************/

:- module(screen,
          [ screen_panel_size/1,
            screen_panel_location/1,
            init_screen/2,
            print_to_screen/2,
            print_to_screen_list/2,
            display_screen/1,
            valid_cursor/1 ]).

:- use_module(flags,
          [ set_flag/1,
            screen_panel_size/1,
            screen_panel_location/1,
            tab_width/1 ]).

:- use_module(misc,
          [ string_of_char_size/3,
            write_at/3,
            panel_huframe_with_title/4,
            panel_hlframe/2 ]).

:- use_module(comp_obj,
          [ get_screen/2,
            get_cursor/2,
            set_screen/2 ]).

:- use_module(error_,
          [ set_error/2,
            set_error/3 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Screen Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set_screen_panel_size(+Lines, +Columns)
% Set the screen size in terms of lines and columns
set_screen_panel_size(Ls, Cs) :-
  set_flag(screen_panel_size(rc(Ls, Cs))).

% init_screen(-Screen, -Cursor)
init_screen(String, lc(0, 0)) :-
  screen_panel_size(rc(Ls, Cs)),
  Size is Ls * Cs,
  string_of_char_size(String, ' ', Size).

% set_cursor(+Cursor, +Comp0--Comp)
% Set the new location of the cursor in the screen
% Fail if out of bounds
set_cursor(Cursor, Comp0-Comp) :-
  Cursor = lc(L, C),
  screen_panel_size(rc(Ls, Cs)),
  (L >= 0, C >= 0, L < Ls, C < Cs
 	 -> comp_obj:set_cursor(Cursor, Comp0-Comp)
 	 ;  Ls1 is Ls - 1,
      Cs1 is Cs - 1,
      atomic_list_concat(['Cursor out of bounds. Max line ', Ls1, ' and column ', Cs1], ErrMsg),
     	set_error('Run', ErrMsg, pos(unknown, unknown)),
     	!,
     	fail).

% print_to_screen(+Command, Comp0-Comp)
% Commands to print: 
%   nl
%   val(Atom)
print_to_screen(nl, Comp0-Comp) :-
  !,
%   screen_panel_size(rc(Ls, _Cs)), This option does not erase contents from the cursor up to end of line
%   get_cursor(Comp0, lc(L, _C)),
%   Ls1 is Ls - 1,
%   (L == Ls1
%    -> scroll_screen(Comp0, Comp1),
%       set_cursor(lc(L, 0), Comp1-Comp)
%    ;  L1 is L+1,
%       set_cursor(lc(L1, 0), Comp0-Comp)).
  screen_panel_size(rc(_Ls, Cs)), % This option erases contents from the cursor up to end of line
  get_cursor(Comp0, lc(_L, C)),
  Size is Cs - C,
  string_of_char_size(Blanks, ' ', Size),
  print_string_to_screen(Blanks, Comp0-Comp).
print_to_screen(tab, Comp0-Comp) :-
  !,
  get_cursor(Comp0, lc(_L, C)),
  tab_width(Tab),
  N is Tab - (C mod Tab),
  format(atom(Spaces), '~|~` t~*+', [N]),
  print_to_screen(val(Spaces), Comp0-Comp).
print_to_screen(val(Number), Comp0-Comp) :-
  number(Number),
  !,
  format_number(Number, String),
  print_string_to_screen(String, Comp0-Comp).
print_to_screen(val(String), Comp0-Comp) :-
  atom(String),
  !,
  print_string_to_screen(String, Comp0-Comp).
print_to_screen(val(Term), Comp0-Comp) :-
  % term(Term),
  !,
  term_to_atom(Term, String),
  print_string_to_screen(String, Comp0-Comp).
print_to_screen(_, Comp-Comp) :-
  set_error('Runtime', 'Unsupported print parameter').

% print_to_screen_list(+Commands, Comp0-Comp)
print_to_screen_list([], Comp-Comp) :-
  !.
print_to_screen_list([Command|Commands], Comp0-Comp) :-
  print_to_screen(Command, Comp0-Comp1),
  print_to_screen_list(Commands, Comp1-Comp).

print_string_to_screen(String, Comp0-Comp) :-
  screen_panel_size(rc(Ls, Cs)),
  Size is Ls * Cs,
  get_screen(Comp0, Screen),
  get_cursor(Comp0, Cursor),
  cursor_to_index(Cursor, I),
  atom_length(String, Length),
  I1 is I + Length,
  (I1 < Size
   -> sub_atom(Screen, 0, I, _, CellsToCursor),
      sub_atom(Screen, I1, _, _, CellsAfterString),
      concat_atom([CellsToCursor, String, CellsAfterString], Screen1),
      sub_atom(Screen1, 0, Size, _, NewScreen),
      index_to_cursor(I1, NewCursor)
   ;  sub_atom(Screen, 0, I, _, CellsToCursor),
      PadR is Cs - I1 mod Cs,
      string_of_char_size(BlankLine, ' ', Cs),
      sub_atom(BlankLine, 0, PadR, _, PadStr),
      concat_atom([CellsToCursor, String, PadStr], Screen1),
      LScreen1 is I + Length + PadR,
      CutL is LScreen1 - Size,
      sub_atom(Screen1, CutL, Size, _, NewScreen),
      I2 is Size - PadR,
      index_to_cursor(I2, NewCursor)),
  set_cursor(NewCursor, Comp0-Comp1),
  set_screen(NewScreen, Comp1-Comp).


% scroll_screen(Comp0, Comp) :-
%   screen_panel_size(rc(Ls, Cs)),
%   Length is Cs * (Ls - 1),
%   get_screen(Comp0, Screen),
%   sub_atom(Screen, Cs, Length, _After, BeginNewScreen),
%   string_of_char_size(BlankLine, ' ', Cs),
%   atom_concat(BeginNewScreen, BlankLine, NewScreen),
%   set_screen(Comp0, NewScreen, Comp).


% display_screen(Comp)
% Display the screen in the terminal
display_screen(Comp) :-
  screen_panel_location(lc(L, C)),
  display_screen(L, C, Comp).

% display_screen(Comp)
% Display the screen in the terminal located 
%   at line Line and Columun (0-based)
display_screen(Line, Column, Comp) :-
  screen_panel_size(rc(Ls, Cs)),
  Width is Cs + 2,
  panel_huframe_with_title('SCREEN', Width, false, UFrame),
  write_at(Line, Column, UFrame),
  get_screen(Comp, Screen),
  Line1 is Line + 1,
  display_screen_lines(Line1, Column, Screen),
  Line2 is Line1 + Ls,
  panel_hlframe(Width, LFrame),
  write_at(Line2, Column, LFrame),
  get_cursor(Comp, Cursor),
  Column1 is Column + 1,
  write_at(Line2, Column1, Cursor).


% display_screen_lines(+Screen)
display_screen_lines(Line, Column, Screen) :-
  screen_panel_size(rc(Ls, Cs)),
  display_screen_lines(Line, Column, Screen, 0, Ls, Cs).

display_screen_lines(_Line, _Column, _Screen, Ls, Ls, _Cs) :-
  !.
display_screen_lines(Line, Column, Screen, L, Ls, Cs) :-
  I is L * Cs,
  sub_atom(Screen, I, Cs, _, ScreenLine),
  atomic_list_concat(['|', ScreenLine, '|'], String),
  write_at(Line, Column, String),
  L1 is L + 1,
  Line1 is Line+1,
  display_screen_lines(Line1, Column, Screen, L1, Ls, Cs).


% valid_cursor(+lc(L, C))
valid_cursor(lc(L, C)) :-
  screen_panel_size(rc(SL, SC)),
  L1 is SL - 1,
  C1 is SC - 1,
  between(0, L1, L),
  between(0, C1, C).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cursor_to_index(+lc(L, C), -I)
% Convert the given line and column numbers to an index on the screen (0 - length(Screen) - 1)
cursor_to_index(lc(L, C), I) :-
  screen_panel_size(rc(_Ls, Cs)),
  I is L * Cs + C.

% index_to_cursor(I, lc(L, C))
% Convert a screen index into line and column numbers
index_to_cursor(I, lc(L, C)) :-
  screen_panel_size(rc(_Ls, Cs)),
  L is floor(I / Cs),
  C is I mod Cs.

% format_number(+Number, -Atom)
%   Numbers have 6 digit-precision
% cf. https://legacy.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
%     https://lists.nongnu.org/archive/html/gcl-devel/2012-10/pdfkieTlklRzN.pdf
%     https://jvns.ca/blog/2023/02/08/why-does-0-1-plus-0-2-equal-0-30000000000000004/
% An integer of more than 6 digits
format_number(Number, Atom) :-
  integer(Number),
  ( Number >= 0, Number <  1_000_000
  ; Number =< 0, Number > -1_000_000),
  !,
  atom_number(SAtom, Number),
  format_sign(Number, SAtom, Atom).
% A float number with 6 integer digits
%   It may be represented as an integer with only 6 digits 
%     (e.g., 999_999.1 is written as ' 999999')
%   or with exponential notation if its digits
%     (e.g., 999_999.5 is written as ' 1E06')
format_number(Number, Atom) :-
  float(Number),
  ( Number >= 0, Number >  999_999
  ; Number =< 0, Number < -999_999),
  !,
  Integer is integer(Number),
  format_number(Integer, Atom).
% A float number in [-1,1[ requires to remove the integer digit 0
format_number(Number, Atom) :-
  float(Number),
  Number < 1, Number > -1,
  !,
  truncate(Number, 6, TNumber),
  atom_number(ZAtom, TNumber),
  ( atom_concat('-0', NFrac, ZAtom),
    atom_concat('-', NFrac, Frac)
  ; atom_concat('0', Frac, ZAtom)),
  !,
  format_sign(Number, Frac, Atom).
% A number with absolute value greater than 1_000_000
% is represented in exponential notation
format_number(Number, Atom) :-
  ( Number >= 0, Number >=  1_000_000
  ; Number =< 0, Number =< -1_000_000),
  !,
  with_output_to_codes(format('~6G', Number), Codes),
  atom_codes(SAtom, Codes),
  format_sign(Number, SAtom, Atom).
% A number with absolute value less than or equal to 999_999 
% is represented with 6 digits, truncating the decimal part
% if needed
format_number(Number, Atom) :-
  with_output_to_codes(format('~g', Number), Codes),
  atom_codes(SAtom, Codes),
  format_sign(Number, SAtom, Atom).

% format_sign(+Number, +SAtom, -Atom)
% Add a preceding blank to positive numbers
format_sign(Number, SAtom, Atom) :-
  (Number >= 0
   -> atom_concat(' ', SAtom, Atom)
   ;  Atom = SAtom).

% truncate(+X, +N, -Result)
% Truncate the number X to N decimal places
truncate(X, N, Result) :-
  (X >= 0
   -> Result is floor(10^N*X)/10^N
   ;  Result is ceiling(10^N*X)/10^N).

