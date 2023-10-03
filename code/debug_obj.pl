/*********************************************************/
/*                                                       */
/* DEBUG_OBJ module for the BASIC language               */
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

:- module(debug_obj,
          [ init_debug/2,
            update_program_listing/2,
            get_screen_panel_location/2,
            get_screen_panel_size/2,
            get_inspect_panel_location/2,
            get_inspect_panel_size/2,
            get_inspect_display_offset/2,
            get_program_panel_location/2,
            get_program_panel_size/2,
            get_program_display_offset/2,
            get_control_panel_location/2,
            get_in_focus/2,
            get_program_lines/2,
            get_program_listing/2,
            get_inspect_listing/2,
            get_listing_line/3,
            get_rownbr/3,
            get_breakpoints/2,
            get_interface_status/2,
            set_program_panel_location/2,
            set_program_panel_size/2,
            set_screen_panel_location/2,
            set_screen_panel_size/2,
            set_inspect_panel_location/2,
            set_inspect_panel_size/2,
            set_control_panel_location/2,
            set_control_panel_size/2,
            set_line_number_width/2,
            set_program_display_offset/2,
            set_inspect_display_offset/2,
            set_program_listing/2,
            set_in_focus/2,
            set_breakpoints/2,
            set_interface_status/2,
            in_focus/3
          ]).


:- use_module(comp_obj,
          [ get_statement_and_next_line/4
          ]).

:- use_module(list,
          [ format_statement/4,
            format_expr/3 ]).

:- use_module(inspect,
          [ inspect_listing/4 ]).

:- use_module(flags,
          [ set_flag/1 ]).

:- use_module(misc,
          [ split_at/4,
            blank_rows/3,
            blank_row/2 ]).

% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).


% Debug object components
%
%   - For the panels, the following keys:
%       program_panel_location: lc(Line, Column)
%       program_panel_size: rc(Rows, Columns)
%       screen_panel_location: lc(Line, Column)
%       screen_panel_size: rc(Lines, Columns)
%       inspect_panel_location: lc(Line, Column)
%       inspect_panel_size: rc(Rows, Columns)
%       control_panel_location: lc(Line, Column)
%       control_panel_size: rc(Rows, Columns)
%       program_display_offset: NbrRows
%       inspect_display_offset: NbrRows
%       in_focus: PanelName (program/inspect)
%   - program_lines: The program lines as an AVT 
%       Key  : Statement line number (Label)
%       Value: RowNbr    % Number of the row in the listing (base-0)
%   - program_listing: The program listing as a list of rows
%   - inspect_vars: Ordered set of inspect var names
%   - inspect_listing: The inspect listing as a list of rows
%   - breakpoints: Ordered set of breakpoints (Line-Stmt)
%   - interface_status: renumbered, waiting, running, ended, saved, error(Message), set_breakpoint, unset_breakpoint, panel_moving, panel_resizing, opening
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_screen_panel_location(Debug, Debug.screen_panel_location).

get_screen_panel_size(Debug, Debug.screen_panel_size).

get_inspect_panel_location(Debug, Debug.inspect_panel_location).

get_inspect_panel_size(Debug, Debug.inspect_panel_size).

get_inspect_display_offset(Debug, Debug.inspect_display_offset).

get_program_panel_location(Debug, Debug.program_panel_location).

get_program_panel_size(Debug, Debug.program_panel_size).

get_program_display_offset(Debug, Debug.program_display_offset).

get_control_panel_location(Debug, Debug.control_panel_location).

get_in_focus(Debug, Debug.in_focus).

get_program_lines(Debug, Debug.program_lines).

get_program_listing(Debug, Debug.program_listing).

get_inspect_listing(Debug, Debug.inspect_listing).

get_listing_line(Listing, Label, Line) :-
  get(Listing, Label, Line).

get_rownbr(Lines, Label, RowNbr) :-
  get(Lines, Label, RowNbr).

get_breakpoints(Debug, Debug.breakpoints).

get_interface_status(Debug, Debug.interface_status).

% Same as in comp_obj:
get(S, Var, Val) :-
  get_assoc(Var, S, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_program_panel_location(Location, Debug-Debug.put([program_panel_location:Location])).

set_program_panel_size(Size, Debug-Debug.put([program_panel_size:Size])).

set_screen_panel_location(Location, Debug-Debug.put([screen_panel_location:Location])) :-
  set_flag(screen_panel_location(Location)).

set_screen_panel_size(Size, Debug-Debug.put([screen_panel_size:Size])) :-
  set_flag(screen_panel_size(Size)).

set_inspect_panel_location(Location, Debug-Debug.put([inspect_panel_location:Location])).

set_inspect_panel_size(Size, Debug-Debug.put([inspect_panel_size:Size])).

set_control_panel_location(Location, Debug-Debug.put([control_panel_location:Location])) :-
  set_flag(control_panel_location(Location)).

set_control_panel_size(Size, Debug-Debug.put([control_panel_size:Size])) :-
  set_flag(control_panel_size(Size)).

set_line_number_width(Width, Debug-Debug.put([line_number_width:Width])).

set_program_display_offset(Offset, Debug-Debug.put([program_display_offset:Offset])).

set_inspect_display_offset(Offset, Debug-Debug.put([inspect_display_offset:Offset])).

set_program_listing(Rows, Debug-Debug.put([program_listing:Rows])).

set_program_lines(Lines, Debug-Debug.put([program_lines:Lines])).

set_in_focus(Panel, Debug-Debug.put([in_focus:Panel])).
%  :-
%   set_flag(in_focus(Panel)).

set_breakpoints(Breakpoints, Debug-Debug.put([breakpoints:Breakpoints])).

set_interface_status(Status, Debug-Debug.put([interface_status:Status])).

% Same as in comp_obj:
% set(S0, Var, Val, S) :-
%   put_assoc(Var, S0, Val, S),
%   !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INIT Debug object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_debug(+Comp, -Debug) is det
init_debug(Comp, Debug) :-
  flags:program_panel_location(lc(PL, PC)),
  flags:program_panel_size(rc(PLS, PCS)),
  screen:screen_panel_location(lc(SL, SC)),
  screen:screen_panel_size(rc(SLS, SCS)),
  flags:inspect_panel_location(lc(IL, IC)),
  flags:inspect_panel_size(rc(ILS, ICS)),
  control:control_panel_location(lc(CL, CC)),
  control:control_panel_size(rc(CLS, CCS)),
  flags:line_number_width(LineNumberWidth), % 00000:00
  program_listing(Comp, rc(PLS, PCS), LineNumberWidth, ProgramLines, ProgramListing),
  InspectVars = [],
  inspect_listing(Comp, rc(ILS, ICS), InspectVars, InspectListing),
  Debug = debug{
    program_panel_location: lc(PL, PC),
    program_panel_size: rc(PLS, PCS),
    screen_panel_location: lc(SL, SC),
    screen_panel_size: rc(SLS, SCS),
    inspect_panel_location: lc(IL, IC),
    inspect_panel_size: rc(ILS, ICS),
    control_panel_location: lc(CL, CC),
    control_panel_size: rc(CLS, CCS),
    line_number_width: LineNumberWidth,
    program_display_offset: 0,
    inspect_display_offset: 0,
    in_focus: program,
    program_lines: ProgramLines, 
    program_listing: ProgramListing, 
    breakpoints: [], 
    inspect_vars: InspectVars, 
    inspect_listing: InspectListing,
    interface_status: waiting}.


% update_program_listing(+Comp, +Debug0--Debug)
update_program_listing(Comp, Debug0-Debug) :-
  get_program_panel_size(Debug0, Size),
  flags:line_number_width(LineNumberWidth),
  program_listing(Comp, Size, LineNumberWidth, ProgramLines, ProgramListing),
  set_program_lines(ProgramLines, Debug0-Debug1),
  set_program_listing(ProgramListing, Debug1-Debug).

% program_listing(+Comp, +ProgramPanelSize, +LineNumberWidth, -Lines, -Rows)
program_listing(Comp, rc(NbrRows, NbrColumns), LineNumberWidth, Lines, Rows) :-
  StatementWidth is NbrColumns - LineNumberWidth - 1,
  min_assoc(Comp.program, Label, _),
  empty_assoc(EmptyLines),
  list_program_to_listing(Comp, NbrRows, NbrColumns, StatementWidth, Label, 0, EmptyLines-Lines, Rows-_).

% list_program_to_listing(+Comp, +Height, +Width, +StmtWidth, +Label, +RowNbr, -PLines, -PRows)
list_program_to_listing(_Comp, Height, Width, _StmtWidth, Label, RowNbr, PL0-PL, PR-PR) :-
  Label == void-void,
  !,
  put_assoc(Label, PL0, RowNbr, PL),
  blank_rows(Height, Width, PR).
list_program_to_listing(Comp, Height, Width, StmtWidth, Label, RowNbr, PL0-PL, PR0-PR) :-
  get_statement_and_next_line(Comp, Label, Statement, NextLabel),
  format_statement(Statement, true, CodesStmt, []),
  format_label_stmt_in_width(CodesStmt, Label, StmtWidth, true, FStmtList),
  length(FStmtList, NbrRows),
  put_assoc(Label, PL0, RowNbr, PL1),
  append(FStmtList, PR1, PR0),
  RowNbr1 is RowNbr + NbrRows,
  list_program_to_listing(Comp, Height, Width, StmtWidth, NextLabel, RowNbr1, PL1-PL, PR1-PR).

% format_label_stmt_in_width(+Codes, +Label, +Width, +IsFirst, -FStmtList)
format_label_stmt_in_width([], _Line_Stmt, _Width, _IsFirst, []) :-
  !.
format_label_stmt_in_width(Codes0, Line-Stmt, Width, IsFirst, [FStmt|FStmts]) :-
  split_at(Width, Codes0, Codes1, RemCodes),
  atom_codes(Statement, Codes1),
  (IsFirst
   -> format(atom(FStmt), '~|~`0t~d~5+:~|~`0t~d~2+ ~w', [Line, Stmt, Statement])
   ;  format(atom(FStmt), '         ~w', [Statement])),
  format_label_stmt_in_width(RemCodes, Line-Stmt, Width, false, FStmts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in_focus(+Debug, +Panel, -InFocus)
% A panel is in focus (InFocus=true) or not (InFocus=false)
in_focus(Debug, Panel, true) :-
  get_in_focus(Debug, Panel),
  !.
in_focus(_Debug, _Panel, false).

