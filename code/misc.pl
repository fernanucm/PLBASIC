/*********************************************************/
/*                                                       */
/* MISC module for the BASIC language                    */
/*   Developed for SWI-Prolog 8.x and above              */
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

% Determining if in Windows:
% current_prolog_flag(windows, true)

:- module(misc,
          [ label_exp/2,
            line_to_label/2,
            line_to_label_exp/2,
            ceiling_line/3,
            udef_fn_name/1,
            string_of_char_size/3,
            current_position/3,
            is_str_type_var/1,
            state/3,
            cls/0,
            home/0,
            write_at/3,
            write_at/2,
            take_n_at/4,
            split_at/4,
            blank_rows/3,
            blank_row/2,
            display_frame/5,
            panel_huframe_with_title/4,
            panel_hlframe/2,
            erase_panel_background/2,
            my_read_line_to_codes/1
          ]).

:- use_module(color).

:- use_module(flags).

% label_exp(+Label, -ExpLabel)
% Turns a label into its equivalent expression
%   (as needed in GOTO expressions)
label_exp(Line-Statement, int(Line)-int(Statement)).

% line_to_label(+Line, -Label)
% Turn a line number into the equivalent label (adding 1 for its first statement)
line_to_label(Line, Line-1).

% line_to_label_exp(+LineExp, -LabelExp)
% Turn a line number into the equivalent label (adding 1 for its first statement)
line_to_label_exp(LineExp, LineExp-int(1)).

% ceiling_line(+Comp0, +LineStmt, -FLineStmt)
% Find the least upper bound label for the given line
%  If the line is found, return it; otherwise return the next one
%  (there is always a next line because of the last void node)
% Straightforward implementation:
%
% ceiling_line(Comp, Line-Stmt, Line-Stmt) :-
%   get(Comp.program, Line-Stmt, _),
%   !.
% ceiling_line(Comp, Line-Stmt, FLine-FStmt) :-
%   ceiling_line_aux(Comp, Line-Stmt, FLine-FStmt).

% % ceiling_line_aux(+Comp, +LineStmt, FLine-FStmt)
% ceiling_line_aux(Comp, Line-_Stmt, FLine-FStmt) :-
%   gen_assoc(FLine-FStmt, Comp.program, _),
%   FLine @> Line,
%   !.
% ceiling_line_aux(Comp, _LineStmt, FLine-FStmt) :-
%   max_assoc(Comp.program, FLine-FStmt, _).

% More efficient implementation with AVL lookup
ceiling_line(Comp, Key, FKey) :-
  ceiling_line_key(Comp.program, Key, FKey),
    !.
ceiling_line(_, _, void-void).
  
ceiling_line_key(t(K, _, _, L, R), Key, FKey) :-
  compare(Rel, Key, K),
  ceiling_line_key(Rel, Key, K, L, R, FKey).

ceiling_line_key(=, Key, _, _, _, Key).
ceiling_line_key(<, Key, _, Tree, _, FKey) :-
  ceiling_line_key(Tree, Key, FKey).
ceiling_line_key(<, _, K, _, _, K).
ceiling_line_key(>, Key, _, _, Tree, FKey) :-
  ceiling_line_key(Tree, Key, FKey).

% udef_fn_name(+Fn)
% Succeed if Fn is a function name, i.e., an atom starting 
%   with 'fn' and followed by one char at the least
udef_fn_name(Fn) :-
  atom_concat(fn, T, Fn),
  T \== ''.

% string_of_char_size(-String, +Char, +Size)
string_of_char_size(String, Char, Size) :-
  atom_codes(Char, [Code]),
  list_of(Size, Code, Codes),
  atom_codes(String, Codes).

% list_of(+Length, +Element, -List)
% Return a list of Element with the given length
list_of(Length, Code, List) :-
  list_of(0, Length, Code, List, []).

list_of(N, N, _Code) -->
  !,
  [].
list_of(I, N, Code) -->
  [Code],
  {I1 is I + 1},
  list_of(I1, N, Code).

% current_position(-Position)//
% Consult the position of the current token, without consuming it
current_position(Position, [Token:Position|TPs], [Token:Position|TPs]) :-
  !.
current_position(pos(last,last), [], []).

% is_str_type_var(+Variable)
% Succeed if Variable ends in $
is_str_type_var(Variable) :-
  atom_concat(_Name, '$', Variable).

% Inspect the next element in the DCG without consuming it
state(S), [S] -->
  [S].


% take_n_at0(+List, +N, +At, -Sublist)
% Take n elements from List starting at At (base-0)
take_n_at(_Xs, 0, _At, []) :-
  !.
take_n_at([], _N, _At, []) :-
  !.
take_n_at([X|Xs], N, At, [X|Ys]) :-
  At == 0,
  !,
  N1 is N - 1,
  take_n_at(Xs, N1, At, Ys).
take_n_at([_|Xs], N, At, Ys) :-
  At1 is At - 1,
  take_n_at(Xs, N, At1, Ys).


% split_at(+Position, +List, -Head, -Tail)
% Split the list List at position Position (0-based)
split_at(0, L, [], L) :-
  !.
split_at(N, [], Spaces, []) :-
  % N>0
  !,
  length(Spaces, N),
  maplist(=(32), Spaces).
split_at(N, [H|T], [H|L1], L2) :-
	M is N - 1,
	split_at(M, T, L1, L2).


% blank_rows(+Height, +Width, -Rows).
blank_rows(Height, Width, Rows) :-
  blank_row(Width, BlankRow), % Create a row of Width blanks
  findall(R, (between(1, Height, _), R=BlankRow), Rows). % List of Height blank rows

% blank_row(+Width, -BlankRow)
% Create a row of Width blanks
blank_row(Width, BlankRow) :-
  format(atom(BlankRow), '~|~` t~*+', [Width]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Commands for ANSI-enabled terminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cls/0
% Clear the console
cls :-
  write('\33\[2J').

% home/0
% Set the cursor to the top left possition (0,0)
home :-
  write('\33\[H').

% write_at(+Line, +Column, +String)
% Write at line Line, column Column the given string
write_at(Line, Column, String) :-
  ansi_locate_terminal_cursor(Line, Column),
  format('~w', [String]).

% write_at(+Line, +Column)
% Next write is positioned at Line, Column
write_at(Line, Column) :-
  ansi_locate_terminal_cursor(Line, Column).
  
% ansi_locate_terminal_cursor(+Line, +Column)
% Locate the cursor at Line, Column
%   in ANSI-enabled terminals
ansi_locate_terminal_cursor(Line, Column) :-
  current_prolog_flag(windows, true),
  !,
  format('\33[~d;~dH', [Column, Line]).
ansi_locate_terminal_cursor(Line, Column) :-
  % Ubuntu terminals lines and columns are 1-based and interchanged
  Line1 is Line + 1,
  Column1 is Column + 1,
  format('\33[~d;~dH', [Line1, Column1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PANEL predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_frame(+Location, +Height, +UFrame, +ListingLines, +LFrame) is det
% Display a frame located at Position lc(L, C)
display_frame(lc(L, C), Height, UFrame, ListingLines, LFrame) :-
  write_at(L, C, UFrame),
  L1 is L + 1,
  display_frame_lines(ListingLines, L1, C), 
  LB is L + Height + 1,
  write_at(LB, C, LFrame).

% display_frame_lines(+Lines, +L, +C) is det
display_frame_lines([], _L, _C) :-
  !.
display_frame_lines([Line|Lines], L, C) :-
  write_at(L, C),
  format('~w~w~w', ['|', Line, '|']),
  L1 is L + 1,
  display_frame_lines(Lines, L1, C).

% panel_huframe_with_title(+Title, +Width, +InFocus, -Frame)
panel_huframe_with_title(Title, Width, true, Frame) :-
  !,
  text_color(focused, fb(FGColor, BGColor)),
  color(foreground, FGColor, FGCode),
  color(background, BGColor, BGCode),
  Width1 is Width - 1 + 12, % +12 because of ANSI escape commands, which are not visible but they seem to count
  format(atom(Frame), '~w~`-t\33[~d;~dm ~w \33[0m~`-t~*|~w', ['+', FGCode, BGCode, Title, Width1, '+']).
panel_huframe_with_title(Title, Width, false, Frame) :-
  Width1 is Width - 1,
  format(atom(Frame), '~w~`-t ~w ~`-t~*|~w', ['+', Title, Width1, '+']).

% panel_hlframe(+Width, +Frame)
panel_hlframe(Width, Frame) :-
  Width1 is Width - 1,
  format(atom(Frame), '~w~`-t~`-t~*|~w', ['+', Width1, '+']).

% erase_panel_background(+Location, +Size)
erase_panel_background(lc(L, C), rc(SL, SC)) :-
  SC1 is SC + 2,
  blank_row(SC1, Row),
  UL is L + SL + 1,
  foreach(between(L, UL, L1), write_at(L1, C, Row)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTHER predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% my_read_line_to_codes(-Codes0)
% Read (and write) codes up to end-of-line from the current input
% Similar to read_line_to_codes(current_input, Codes), but 
% respecting the screen cursor location
% Only intended to user_input. read_line_to_codes/2 is used 
% otherwise to be able to redirect input from a file (for tests)
my_read_line_to_codes(Codes) :-
  current_input(Stream),
  stream_property(Stream, alias(user_input)),
  !,
  my_read_line_to_codes_aux(Codes0),
  process_read_codes(Codes0, Codes).
my_read_line_to_codes(Codes0) :-
  current_input(Stream),
  read_line_to_codes(Stream, Codes0).

my_read_line_to_codes_aux(Codes0) :-
  get_single_char(Code),
  char_code(Char, Code),
  write(Char),
  (Code == 13
   -> Codes0 = [] % Remove trailing end-of-line
   ;  Codes0 = [Code|Codes],
      my_read_line_to_codes_aux(Codes)).

  
% process_read_codes(+Codes0, -Codes) is det
process_read_codes(Codes0, Codes) :-
  reverse(Codes0, Codes1),
  process_read_codes_aux(Codes1, Codes2),
  reverse(Codes2, Codes).


% process_read_codes_aux(+Codes0, -Codes) is det
process_read_codes_aux([], []) :-
  !.
process_read_codes_aux([8, 8|Codes0], Codes) :-
  !,
  process_read_codes_aux([8|Codes0], Codes1),
  process_read_codes_aux([8|Codes1], Codes).
process_read_codes_aux([8, _Code|Codes0], Codes) :-
  !,
  process_read_codes_aux(Codes0, Codes).
process_read_codes_aux([Code|Codes0], [Code|Codes]) :-
  char_type(Code, print),
  !,
  process_read_codes_aux(Codes0, Codes).
process_read_codes_aux([_Code|Codes0], Codes) :-
  process_read_codes_aux(Codes0, Codes).

