/*********************************************************/
/*                                                       */
/* DEBUG predicates for the BASIC language               */
/*    Version Seiko Data 2000                            */
/*                                                       */
/*                                                       */
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

:- module(debug,
          [ debug/1 ]).

:- use_module(basic,
          [ lex_parse/2,
            lex_parse_transform/2 ]).

:- use_module(parser,
          [ transform/2 ]).

:- use_module(interpreter,
          [ interpret_statement/2 ]).

:- use_module(comp_obj).

:- use_module(debug_obj).

:- use_module(tools).

:- use_module(inspect).

:- use_module(control).

:- use_module(error_).

:- use_module(screen,
          [ display_screen/1,
            print_to_screen/2 ]).

:- use_module(misc,
          [ cls/0,
            write_at/3,
            write_at/2,
            take_n_at/4,
            blank_row/2,
            display_frame/5,
            panel_huframe_with_title/4,
            panel_hlframe/2,
            ceiling_line/3 ]).

:- use_module(color).


% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).

% % Signal handling (e.g., Ctrl+C)
% :- initialization(ensure_loaded('signal.pl')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEBUG Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% debug(+Source) is det
debug(Source) :-
  debug(Source, _Debug).

%% debug(+Input, -Debug) is det
debug(Source, Debug) :-
%   register_signals,
  % Comp and Debug objects are initialized should loading the source fails because of a syntax/lexical error
  init_comp("", Comp0),
  init_debug(Comp0, Debug0),
  reset_error,
  cls,
  debug_loop(init(Source), Comp0-_Comp, Debug0-Debug),
  write_at(18, 0).

% debug_loop(+Status, +Comp0--Comp, +Debug0--Debug) is det
% Debug the program in Comp
debug_loop(quit, Comp-Comp, Debug-Debug) :-
  !.
debug_loop(init(Source), _Comp0-Comp, _Debug0-Debug) :-
  init_objects(Source, Comp0, Debug0),
  !,
  debug_loop(stop, Comp0-Comp, Debug0-Debug).
debug_loop(Status0, Comp0-Comp, Debug0-Debug) :-
  Status0 \= init(_),
  display_panels(Status0, Comp0, Debug0),
  get_and_process_command(Status0-Status1, Comp0-Comp1, Debug0-Debug1),
  debug_statement(Status1-Status, Comp1-Comp2, Debug1-Debug2),
  !,
  debug_loop(Status, Comp2-Comp, Debug2-Debug).
debug_loop(_Status0, Comp0-Comp, Debug0-Debug) :-
  with_output_to_codes(process_error, Codes),
  atom_codes(Error, Codes),
  print_to_screen(val(Error), Comp0-Comp1),
  !,
  debug_loop(stop, Comp1-Comp, Debug0-Debug).
debug_loop(_Status, Comp-Comp, Debug-Debug) :-
  write_at(20, 0),
  process_error,
  !,
  fail.


% display_panels(+DebugStatus, +Comp, +Debug)
% Do not refresh panels while processing:
% display_panels(DebugStatus, Comp, _Debug) :-
%   (DebugStatus == run ; DebugStatus == cont),
%   get_status(Comp, CompStatus),
%   CompStatus \== end,
%   CompStatus \== stop,
%   !.
display_panels(_DebugStatus, Comp0, Debug0) :-
  display_listing(Debug0, Comp0),
  display_screen_panel(Debug0, Comp0),
  display_control(Debug0),
  display_inspects(Debug0).

% debug_statement(+Status0--Status, +Comp0--Comp, +Debug0--Debug)
% Interpret statement
% Stop if a breakpoint is found
debug_statement(run-stop, Comp-Comp, Debug0-Debug) :-
  get_line(Comp, Line),
  get_breakpoints(Debug0, Breakpoints),
  ord_memberchk(Line, Breakpoints),
  !,
  set_interface_status(waiting, Debug0-Debug1),
  set_program_display_offset(0, Debug1-Debug).
% Execute the current statement if either running, or continuing or stepping
debug_statement(Status0-Status, Comp0-Comp, Debug0-Debug) :-
  (Status0 == run, Status = run
   ;
   Status0 == cont, Status = run
   ;
   Status0 == step, Status = stop),
  !,
  get_statement_and_update_lines(Statement, Comp0-Comp1),
  interpret_statement(Statement, Comp1-Comp),
  update_debug(Comp, Statement, Debug0-Debug).
% Do not execute the current statement otherwise (Debug status stop, end or quit)
debug_statement(Status-Status, Comp-Comp, Debug-Debug).

% update_debug(+Comp, +Statement, +Debug0--Debug) :-
update_debug(Comp, Statement, Debug0-Debug) :-
  add_inspect(Statement, Comp-_Comp, Debug0-Debug1),
  update_inspect_listing(Comp, Debug1-Debug).

% update_inspect_listing(+Comp, +Debug0--Debug) :-
update_inspect_listing(Comp, Debug-Debug.put([inspect_listing:InspectListing])) :-
  inspect_listing(Comp, Debug.inspect_panel_size, Debug.inspect_vars, InspectListing).

% add_inspect(+Statement, +Comp0--Comp, +Debug0--Debug1)
add_inspect(let(Id, _E), Comp0-Comp, Debug0-Debug) :-
  !,
  add_inspect_var(Id, Comp0-Comp, Debug0-Debug).
add_inspect(for(Id, _Start, _End, _Step, _OptStep), Comp0-Comp, Debug0-Debug) :-
  !,
  add_inspect_var(Id, Comp0-Comp, Debug0-Debug).
add_inspect(input(_MsgExpr, Variables), Comp0-Comp, Debug0-Debug) :-
  !,
  add_inspect_vars(Variables, Comp0-Comp, Debug0-Debug).
add_inspect(read(Variables), Comp0-Comp, Debug0-Debug) :-
  !,
  add_inspect_vars(Variables, Comp0-Comp, Debug0-Debug).
add_inspect(_, Comp-Comp, Debug-Debug).

% add_inspect_var(+Id, +Comp0--Comp, +Debug0--Debug)
add_inspect_var(Id, Comp0-Comp, Debug0-Debug) :-
  add_inspect_vars([Id], Comp0-Comp, Debug0-Debug).

% add_inspect_vars(+Ids, +Comp0--Comp, +Debug0--Debug)
add_inspect_vars(Ids, Comp0-Comp, Debug-Debug.put([inspect_vars:InspectVars])) :-
  eval_inspect_vars(Ids, EIds, Comp0-Comp),
  ord_union(Debug.inspect_vars, EIds, InspectVars).

% eval_inspect_vars(+Ids, -EIds, +Comp0--Comp)
eval_inspect_vars([], [], Comp-Comp) :-
  !.
eval_inspect_vars([Id|Ids], [EArray|EVs], Comp0-Comp) :-
  interpreter:id_array_name_index(Id, Name, Index),
  !,
  interpreter:eval_exps(Index, Is, Comp0-Comp1),
  EArray =.. [Name|Is],
  eval_inspect_vars(Ids, EVs, Comp1-Comp).
eval_inspect_vars([id(Id)|Ids], [Id|EVs], Comp0-Comp1) :-
  eval_inspect_vars(Ids, EVs, Comp0-Comp1).


% init_objects(+Source, -Comp, -Debug)
init_objects(Source, Comp, Debug) :-
  init_comp(Source, Comp0),
  init_debug(Comp0, Debug),
  set_status(stop, Comp0-Comp).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display listing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_listing(+Debug, +Comp)
display_listing(Debug, Comp) :-
  get_line(Comp, Label),
  get_program_panel_location(Debug, Location),
  get_program_panel_size(Debug, rc(TotalRows, Columns)),
  get_program_display_offset(Debug, DisplayOffset),
  Width is Columns + 2,
  in_focus(Debug, program, InFocus),
  get_source(Comp, Source),
  program_file_banner(Debug, Source, Banner),
  panel_huframe_with_title(Banner, Width, InFocus, UFrame),
  listing_lines(Debug, Label, DisplayOffset, TotalRows, ListingLines),
  panel_hlframe(Width, LFrame),
  display_frame(Location, TotalRows, UFrame, ListingLines, LFrame).

program_file_banner(_Debug, Source, 'PROGRAM') :-
  is_list(Source),
  !.
program_file_banner(Debug, Source, Banner) :-
  atom_concat('PROGRAM ', Source, Banner0),
  get_program_panel_size(Debug, rc(_, C)),
  L is C - 6,
  atom_codes(Banner0, CodesBanner0),
  take_n_at(CodesBanner0, L, 0, CodesBanner1),
  atom_codes(Banner, CodesBanner1),
  !.

% listing_lines(+Debug, +Label, +Offset, +TotalRows, -Rows)
% Return ListingLines from a Listing, starting at Label with an Offset
%   for a total of TotalRows (height)
listing_lines(Debug, Label, Offset, TotalRows, Rows) :-
  get_program_lines(Debug, Lines),
  get_program_listing(Debug, Listing),
  get_rownbr(Lines, Label, RowNbr),
  StartRowNbr is max(RowNbr - Offset, 0),
  take_n_at(Listing, TotalRows, StartRowNbr, Rows0),
  HRow is RowNbr - StartRowNbr,
  get_program_panel_size(Debug, rc(_, Width)),
  blank_row(Width, BlankRow),
  highlight_row(Rows0, HRow, TotalRows, BlankRow, Rows).

% highlight_row(+Rows0, +HRow, +TotalRows, +BlankRow, -Rows)
% Highlight the row corresponding to the current program line
highlight_row(Rows, HRow, _TotalRows, _BlankRow, Rows) :-
  HRow < 0,
  !.
highlight_row(Rows, HRow, TotalRows, _BlankRow, Rows) :-
  HRow >= TotalRows,
  !.
highlight_row(Rows0, HRow, _TotalRows, BlankRow, Rows) :-
  highlight_row_aux(Rows0, 0, HRow, BlankRow, Rows).

%highlight_row_aux(+Rows0, +CurrentRow, +HighRow, +BlankRow, -Rows)
% highlight_row_aux([], _Row, _HRow, _BlankRow, []) :-
%   !.
highlight_row_aux([Row0|Rows0], HRow, HRow, BlankRow, [Row|Rows]) :-
  !,
  apply_text_color(Row0, highlighted, Row),
  % A statement may span several lines 
  % (starting with blanks instead of a line number)
  highlight_next_rows(Rows0, BlankRow, Rows).
highlight_row_aux([Row0|Rows0], CRow0, HRow, BlankRow, [Row0|Rows]) :-
  CRow is CRow0 + 1,
  highlight_row_aux(Rows0, CRow, HRow, BlankRow, Rows).

highlight_next_rows([], _BlankRow, []) :-
  !.
highlight_next_rows([BlankRow|Rows], BlankRow, [BlankRow|Rows]) :-
  !. % Do not highlight blank rows
highlight_next_rows([Row0|Rows0], BlankRow, [Row|Rows]) :-
  atom_concat(' ', _, Row0),
  !,
  apply_text_color(Row0, highlighted, Row),
  highlight_next_rows(Rows0, BlankRow, Rows).
highlight_next_rows(Rows, _BlankRow, Rows).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display screen panel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_screen_panel(+Debug, +Comp)
display_screen_panel(Debug, Comp) :-
  display_screen(Comp), % From interpreter
  % Overwrite the (possibly focused) heading
  get_screen_panel_location(Debug, lc(L, C)),
  get_screen_panel_size(Debug, rc(_, Width0)),
  Width is Width0 + 2,
  in_focus(Debug, screen, InFocus),
  panel_huframe_with_title('SCREEN', Width, InFocus, UFrame),
  write_at(L, C, UFrame).
