/*********************************************************/
/*                                                       */
/* CONTROL module for the BASIC language                 */
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

:- module(control,
          [ get_and_process_command/3,
            control_panel_size/1,
            control_panel_location/1,
            display_control/1
          ]).

:- use_module(file).

:- use_module(flags,
          [ control_panel_size/1,
            control_panel_location/1,
            line_number_width/1,
            focus_order/1,
            current_open_folder/1
          ]).

:- use_module(comp_obj,
          [ init_comp/2,
            get_source/2,
            get_line/2,
            get_last/3,
            get_status/2,
            get_program_start_line/2,
            set_screen/2,
            set_cursor/2,
            set_line/2,
            set_status/2
          ]).

:- use_module(debug_obj).

:- use_module(tools).

:- use_module(misc,
          [ display_frame/5,
            panel_huframe_with_title/4,
            panel_hlframe/2,
            write_at/2,
            write_at/3,
            split_at/4,
            blank_row/2,
            cls/0
          ]).


%
% Debugger commands:
%
%  RUN:       Execute the program up to its end or first breakpoint
%  STEP:      Execute the current statement
%  STOP:      Stop the execution
%  CONT:      Continue the execution
%  SETBK:     Set breakpoint in the current (highlighted) line
%  RESET:     Reset the debugging session (reload the program)
%  RENUM:     Renum the program and reset
%  OPEN:      Open a file from a selectable folder
%  SAVE:      Save the loaded program (useful after a renumbering)
%  QUIT:      Quit the debugger
%  REFRESH:   Clear the host screen and redraw
%  TAB:       Set focus to the program or inspect panel
%  RESIZE:    Let the panel in focus to be resized with the arrow keys
%  MOVE:      Let the panel in focus to be moved with the arrow keys
%  ArrowUp:   Scroll up/Vertical Downsize/Move Up the panel in focus
%  ArrowDown: Scroll down/Vertical Upsize/Move Down the panel in focus
%  ArrowLeft: Horizontal Downsize/Move Left the panel in focus
%  ArrowRight:Horizontal Upsize/Move Right the panel in focus
%  PageUp:    Page up the panel in focus
%  PageDown:  Page down the panel in focus
%  Home:      Go to the first line of the panel in focus
%  End:       Go to the last line of the panel in focus
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTROL Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_and_process_command(+OldStatus--NewStatus, +Comp0--Comp, +Debug0--Debug)
% Get and process a command from user input along debugging
% Commands cannot be read while running the BASIC program (Status=run),
% only when execution is paused because of the debugger or
% when the program is finished
get_and_process_command(_DebugStatus-end, Comp0-Comp, _Debug0-Debug) :-
  get_status(Comp0, new),
  !,
  set_status(end, Comp0-Comp),
  init_debug(Comp, Debug).
get_and_process_command(DebugStatus0-DebugStatus, Comp0-Comp, Debug0-Debug) :-
  (DebugStatus0 == run ; DebugStatus0 == cont),
  get_status(Comp0, CompStatus),
  CompStatus \== end,
  CompStatus \== stop,
  !,
  (DebugStatus0 == run
   -> get_and_process_inkey_command(DebugStatus0-DebugStatus, Comp0-Comp, Debug0-Debug)
   ;  DebugStatus = DebugStatus0,
      Comp = Comp0,
      Debug = Debug0).
get_and_process_command(DebugStatus-NewDebugStatus, Comp0-Comp, Debug0-Debug) :-
  update_interface_status(Comp0, Debug0-Debug1),
  get_command(Debug0, Command),
  process_command(Command, DebugStatus-NewDebugStatus, Comp0-Comp, Debug1-Debug).

% get_and_process_inkey_command(+DebugStatus0--DebugStatus, +Comp0--Comp, +Debug0--Debug)
% Get an inkey command (e.g., S(T)OP to stop a running program)
get_and_process_inkey_command(DebugStatus0-DebugStatus, Comp0-Comp, Debug0-Debug) :-
  get_inkey_command(Command),
  process_command(Command, DebugStatus0-DebugStatus, Comp0-Comp, Debug0-Debug).

% get_inkey_command(-Command)
% Inspect if there is a key pressed and return the corresponding inkey command
get_inkey_command(Command) :-
  get_inkey(Code),
  code_command(Code, Command).

% get_inkey(-Code)
% Read the key pressed
get_inkey(Code) :-
  % Not possible yet in Windows
  current_prolog_flag(windows, true),
  !,
  Code = 0.
get_inkey(Code) :-
  % Linux/macOS
  wait_for_input([user_input], [], 0),
  !,
  Code = 0.
get_inkey(Code) :-
  get_code(Code).


% process_command(+Command, +DebugStatus--NewDebugStatus, +Comp0--Comp, +Debug0--Debug)
% RUN Command
process_command(run, _Status-run, Comp0-Comp, Debug0-Debug) :-
  !,
  set_interface_status(running, Debug0-Debug),
  get_program_start_line(Comp0.program, Label),
  set_line(Label, Comp0-Comp1),
  set_status(run, Comp1-Comp).
% STEP Command
process_command(step, _Status-step, Comp-Comp, Debug-Debug) :-
  !.
% STOP Command
process_command(stop, _DebugStatus-cont, Comp0-Comp, Debug-Debug) :-
  (get_status(Comp0, run)
   -> set_status(stop, Comp0-Comp)
   ;  true),
   !.
% CONT Command
process_command(cont, _DebugStatus-cont, Comp0-Comp, Debug-Debug) :-
  get_status(Comp0, CompStatus),
  CompStatus \== end,
  !,
  set_status(run, Comp0-Comp).
process_command(cont, _Status-end, Comp-Comp, Debug-Debug) :-
  !.
% SETBK Command
process_command(setbk, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  line_at_top_program_listing(Comp, Debug0, Label),
  toggle_breakpoint(Label, Debug0-Debug).
% RESET Command
process_command(reset, _Status-stop, Comp0-Comp, _-Debug) :-
  !,
  get_source(Comp0, Source),
  debug:init_objects(Source, Comp, Debug).
% RENUM Command
process_command(renum, _Status-stop, Comp0-Comp, _-Debug) :-
  !,
  get_source(Comp0, Source),
  renum_source_comp(Source, 10, 10, Comp1),
  init_debug(Comp1, Debug0),
  set_status(stop, Comp1-Comp),
  set_interface_status(renumbered, Debug0-Debug).
% OPEN File Panel Command
process_command(open, Status-Status, Comp0-Comp, Debug0-Debug) :-
  !,
  current_open_folder(Folder),
  open_file_panel(Folder, Outcome),
  (Outcome = open(Source)
   -> debug:init_objects(Source, Comp, Debug)
   ;
   Outcome == cancel
   -> Comp = Comp0,
      Debug = Debug0
   ;
   Comp = Comp0,
   set_interface_status(error(Outcome), Debug0-Debug)).
% SAVE Command
process_command(save, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_source(Comp, Source),
  (atom(Source)
   -> catch(
       (tell(Source),
        output_program(Comp),
        told,
        set_interface_status(saved, Debug0-Debug)),
        ExceptionTerm,
       (format(atom(Message), 'Error ~w', [ExceptionTerm]),
        set_interface_status(error(Message), Debug0-Debug)))
   ;  true).
% QUIT Command
process_command(quit, _Status-quit, Comp-Comp, Debug-Debug) :-
  !.
% REFRESH Command
process_command(refresh, Status-Status, Comp-Comp, Debug-Debug) :-
  !,
  cls.
% MOVE Panel Commands
process_command(up, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_moving),
  !,
  move_panel_up(Debug0-Debug).
process_command(down, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_moving),
  !,
  move_panel_down(Debug0-Debug).
process_command(left, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_moving),
  !,
  move_panel_left(Debug0-Debug).
process_command(right, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_moving),
  !,
  move_panel_right(Debug0-Debug).
% RESIZE Panel Commands
process_command(up, Status-Status, Comp0-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_resizing),
  !,
  resize_panel_shorten_vertical(Comp0-Comp, Debug0-Debug).
process_command(down, Status-Status, Comp0-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_resizing),
  !,
  resize_panel_enlarge_vertical(Comp0-Comp, Debug0-Debug).
process_command(left, Status-Status, Comp0-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_resizing),
  !,
  resize_panel_shorten_horizontal(Comp0-Comp, Debug0-Debug).
process_command(right, Status-Status, Comp0-Comp, Debug0-Debug) :-
  get_interface_status(Debug0, panel_resizing),
  !,
  resize_panel_enlarge_horizontal(Comp0-Comp, Debug0-Debug).
% SCROLL Panel Commands
process_command(up, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, program),
  get_current_line_rownbr(Comp, Debug0, RowNbr),
  get_program_display_offset(Debug0, Offset),
  RowNbr > Offset,
  !,
  add_offset_to(1, program, Debug0-Debug).
process_command(up, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, inspect),
  get_inspect_display_offset(Debug0, Offset),
  0 > Offset,
  !,
  add_offset_to(1, inspect, Debug0-Debug).
process_command(up, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
process_command(down, Status-Status, Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, program),
  get_last(Debug.program_lines, t, _), % Empty program
  !.
process_command(down, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, program),
  get_line(Comp, CurrentLabel),
  get_last(Debug0.program_lines, LastLabel, _),
  get_program_lines(Debug0, Lines),
  get_rownbr(Lines, LastLabel, LastRowNbr),
  get_rownbr(Lines, CurrentLabel, CurrentRowNbr),
  get_program_display_offset(Debug0, Offset),
  Diff is LastRowNbr - CurrentRowNbr + Offset,
  Diff > 1,
  !,
  add_offset_to(-1, program, Debug0-Debug).
process_command(down, Status-Status, Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, inspect),
  get_inspect_listing(Debug, []), % Empty inspect
  !.
process_command(down, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, inspect),
  get_inspect_listing(Debug0, InspectListing),
  length(InspectListing, TotalRows),
  get_inspect_display_offset(Debug0, Offset),
  get_inspect_panel_size(Debug0, rc(Rows, _)),
  Offset > Rows + 1 - TotalRows,
  !,
  add_offset_to(-1, inspect, Debug0-Debug).
process_command(down, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
% PAGEUP Panel Program Command
process_command(pageup, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_current_line_rownbr(Comp, Debug0, RowNbr),
  get_program_display_offset(Debug0, Offset),
  get_program_panel_size(Debug0, rc(PanelRows, _)),
  Disp is min(PanelRows, RowNbr - Offset),
  add_offset(Disp, Debug0-Debug).
% PAGEDOWN Panel Program Command
process_command(pagedown, Status-Status, Comp-Comp, Debug0-Debug) :-
  get_line(Comp, CurrentLabel),
  get_last(Debug0.program_lines, LastLabel, _),
  get_program_lines(Debug0, Lines),
  get_rownbr(Lines, LastLabel, LastRowNbr),
  get_rownbr(Lines, CurrentLabel, CurrentRowNbr),
  get_program_display_offset(Debug0, Offset),
  get_program_panel_size(Debug0, rc(PanelRows, _)),
  Disp is min(PanelRows, LastRowNbr - CurrentRowNbr + Offset - 1),
  add_offset(-Disp, Debug0-Debug).
% RESIZE Command
process_command(resize, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  toggle_resize(Debug0-Debug).
% MOVE Command
process_command(move, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  toggle_move(Debug0-Debug).
% TAB Command
process_command(tab, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  get_in_focus(Debug0, InFocus),
  next_in_focus(InFocus, NewInFocus),
  set_in_focus(NewInFocus, Debug0-Debug).
% SPY Command
process_command(spy, Status-Status, Comp-Comp, Debug-Debug) :-
  !,
  debug,
  spy(control:deb).
% HOME Command
process_command(home, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  get_current_line_rownbr(Comp, Debug0, RowNbr),
  Offset is -RowNbr,
  set_program_display_offset(Offset, Debug0-Debug).
% END Command
process_command(end, Status-Status, Comp-Comp, Debug0-Debug) :-
  !,
  get_line(Comp, CurrentLabel),
  get_last(Debug0.program_lines, LastLabel, _),
  get_program_lines(Debug0, Lines),
  get_rownbr(Lines, CurrentLabel, CurrentRowNbr),
  get_rownbr(Lines, LastLabel, LastRowNbr),
  Offset is CurrentRowNbr - LastRowNbr + 1,
  set_program_display_offset(Offset, Debug0-Debug).
% Uncatched Arrow Commands
process_command(up, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
process_command(down, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
process_command(left, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
process_command(right, Status-Status, Comp-Comp, Debug-Debug) :-
  !.
% VOID Command
process_command(void, Status-Status, Comp-Comp, Debug-Debug) :-
  !.

% next_in_focus(+InFocus, -NewInFocus) is det
next_in_focus(InFocus, NewInFocus) :-
  focus_order(FocusOrder),
  (append(_, [InFocus, NewInFocus|_], FocusOrder), !
   ; FocusOrder = [NewInFocus|_]).
  
% add_offset(+Offset, +Debug0--Debug)
add_offset(Offset, Debug0-Debug) :-
  get_in_focus(Debug0, InFocus),
  (InFocus == program
   -> add_offset_to(Offset, program, Debug0-Debug)
   ;  add_offset_to(Offset, inspect, Debug0-Debug)).

% add_offset_to(+Offset, +Panel, +Debug0--Debug)
add_offset_to(Offset, program, Debug0-Debug) :-
  get_program_display_offset(Debug0, CurrentOffset),
  NewOffset is CurrentOffset + Offset,
  set_program_display_offset(NewOffset, Debug0-Debug).
add_offset_to(Offset, inspect, Debug0-Debug) :-
  get_inspect_display_offset(Debug0, CurrentOffset),
  NewOffset is CurrentOffset + Offset,
  set_inspect_display_offset(NewOffset, Debug0-Debug).

% get_command(+Debug, -Command)
% Get a command from user input along debugging
get_command(Debug, Command) :-
  read_interface_command(Debug, Command).


% update_interface_status(+Comp, +Debug0--Debug)
update_interface_status(Comp, Debug0-Debug) :-
  get_status(Comp, Status),
  Status == end,
  get_interface_status(Debug0, running),
  !,
  set_interface_status(ended, Debug0-Debug),
  display_control(Debug).
update_interface_status(_Comp, Debug-Debug).


% interface_message(+Status, -Message).
% Messages for the interface status
interface_message(renumbered, 'Program renumbered').
interface_message(waiting, 'Waiting command').
interface_message(running, 'Program running').
interface_message(ended, 'Program ended').
interface_message(saved, 'Program saved').
interface_message(error(Message), Message).
interface_message(set_breakpoint, 'Breakpoint set').
interface_message(unset_breakpoint, 'Breakpoint unset').
interface_message(panel_moving, 'Moving panel').
interface_message(panel_resizing, 'Resizing panel').
interface_message(opening, 'Opening a file').

% % write_interface_message(+Message)
% write_interface_message(Message) :-
%   control_panel_size(rc(_, Width0)),
%   Width is Width0 + 10,
%   move_cursor_below_control_panel(0),
%   format(atom(Line), 'Status : ~w~` t~*|', [Message, Width]),
%   write(Line).

% read_interface_command(+Debug, -Command)
read_interface_command(Debug, Command) :-
  get_panel_size(Debug, control, rc(SL, _)),
  get_panel_location(Debug, control, lc(L, C)),
  L1 is L + SL,
  C1 is C + 10,
  write_at(L1, C1),
  get_single_char(CharCode),
  code_command(CharCode, Command).

% code_command(+Char, -Command)
% Return the command corresponding to the read character
code_command(114, run)      :- !. % r
code_command(115, step)     :- !. % s
code_command(116, stop)     :- !. % t
code_command(99,  cont)     :- !. % c
code_command(98,  setbk)    :- !. % b
code_command(101, reset)    :- !. % e
code_command(110, renum)    :- !. % n
code_command(118, save)     :- !. % v
code_command(113, quit)     :- !. % q
code_command(109, move)     :- !. % m
code_command(122, resize)   :- !. % z
code_command(111, open)     :- !. % o
code_command(104, refresh)  :- !. % h
code_command(9,   tab)      :- !. % Tabulator (no char for back-tabulator)
code_command(2,   left)     :- !. % Left arrow
code_command(6,   right)    :- !. % Right arrow
code_command(16,  up)       :- !. % Up arrow
code_command(14,  down)     :- !. % Down arrow
code_command(117, pageup)   :- !. % Page up
code_command(100, pagedown) :- !. % Page down
code_command(1,   home)     :- !. % Home
code_command(5,   end)      :- !. % End
code_command(112, spy)      :- !. % Spy
code_command(_,   void)     :- !. % A char not related to a command


% display_control(+Debug)
display_control(Debug) :-
  get_panel_location(Debug, control, Location),
  get_panel_size(Debug, control, rc(TotalRows, Columns)),
  Width is Columns + 2,
  in_focus(Debug, control, InFocus),
  panel_huframe_with_title('CONTROL', Width, InFocus, UFrame),
  control_lines(Debug, ControlLines),
  panel_hlframe(Width, LFrame),
  display_frame(Location, TotalRows, UFrame, ControlLines, LFrame).

% control_lines(+Debug, -ControlLines) is det
control_lines(Debug, ControlLines) :-
  get_interface_status(Debug, Status),
  interface_message(Status, Message),
  get_control_panel_size(_, rc(_, Width)),
  format(atom(StatusLine), 'Status : ~w~` t~*|', [Message, Width]),
  ControlLines = [
  ' (R)UN     S(T)OP   (C)ONT ',
  ' SET(B)K   (S)TEP  R(E)SET ',
  ' (Q)UIT   TAB (Move Focus) ',
  ' SA(V)E  (O)PEN  REFRES(H) ',
  ' (M)OVE  RESI(Z)E  RE(N)UM ',
  '       Arrow Up/Down       ',
  ' Page (U)p/(D)own: Paging  ',
  ' Home/End: First/last page ',
  '---------------------------',
  StatusLine,
  'Command:                   '].
% Fixed Control panel size w.r.t. the lines above
% Adjust in flags
get_control_panel_size(_, Size) :-
  control_panel_size(Size).


% move_cursor_below_control_panel(+Offset)
move_cursor_below_control_panel(Offset) :-
  control_panel_size(rc(SL, _)),
  control_panel_location(lc(L, C)),
  L1 is L + SL + 2 + Offset,
  C1 is C + 1,
  write_at(L1, C1).


% line_at_top_program_listing(+Comp, +Debug, -Label) is det
% Return the program line number at the top of the program listing
line_at_top_program_listing(Comp, Debug, TopLabel) :-
  get_line(Comp, Label),
  get_program_lines(Debug, Lines),
  get_rownbr(Lines, Label, RowNbr),
  get_program_display_offset(Debug, Offset),
  gen_assoc(TopLabel, Lines, TopRowNbr),
  DisplayRowNbr is RowNbr - Offset,
  TopRowNbr >= DisplayRowNbr. 


% toggle_resize(+Debug0--Debug)
toggle_resize(Debug0-Debug) :-
  get_interface_status(Debug0, Status),
  (Status == panel_resizing
   -> NewStatus = waiting
   ;  NewStatus = panel_resizing),
  set_interface_status(NewStatus, Debug0-Debug).


% toggle_resize(+Debug0--Debug)
toggle_move(Debug0-Debug) :-
  get_interface_status(Debug0, Status),
  (Status == panel_moving
   -> NewStatus = waiting
   ;  NewStatus = panel_moving),
  set_interface_status(NewStatus, Debug0-Debug).


% toggle_breakpoint(+Label, +Debug0--Debug) is det
toggle_breakpoint(Label, Debug0-Debug) :-
  get_breakpoints(Debug0, Breakpoints0),
  (ord_memberchk(Label, Breakpoints0)
   -> ord_del_element(Breakpoints0, Label, Breakpoints),
      Char = ' ',
      set_interface_status(unset_breakpoint, Debug0-Debug1)
   ;  ord_add_element(Breakpoints0, Label, Breakpoints),
      Char = '*',
      set_interface_status(set_breakpoint, Debug0-Debug1)),
  update_breakpoints(Label, Breakpoints, Char, Debug1-Debug).

% update_breakpoints(+Label, +Breakpoints, +Char, +Debug0--Debug) is det
update_breakpoints(Label, Breakpoints, Char, Debug0-Debug) :-
  set_breakpoints(Breakpoints, Debug0-Debug1),
  get_line_rownbr(Label, Debug1, RowNbr),
  mark_breakpoint(RowNbr, Char, Debug1-Debug).

% mark_breakpoint(+RowNbr, +Char, +Debug0--Debug) is det
mark_breakpoint(RowNbr, Char, Debug0-Debug) :-
  % Replace the row
  get_program_listing(Debug0, Rows0),
  split_at(RowNbr, Rows0, Head, [Row0|Tail]),
  append(Head, [Row|Tail], Rows),
  set_program_listing(Rows, Debug0-Debug),
  % Replace the character in the row
  line_number_width(LineNumberWidth),
  atom_codes(Row0, Codes0),
  split_at(LineNumberWidth, Codes0, CHead, [_|CTail]),
  char_code(Char, Code),
  append(CHead, [Code|CTail], Codes),
  atom_codes(Row, Codes).

% get_current_line_rownbr(+Comp, +Debug, -RowNbr) is det
get_current_line_rownbr(Comp, Debug, RowNbr) :-
  get_line(Comp, Label),
  get_line_rownbr(Label, Debug, RowNbr).

% get_line_rownbr(+Label, +Debug, -RowNbr) is det
get_line_rownbr(Label, Debug, RowNbr) :-
  get_program_lines(Debug, Lines),
  get_rownbr(Lines, Label, RowNbr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PANEL handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% move_panel_up(+Debug0--Debug)
move_panel_up(Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_location(Debug0, Panel, lc(L, C)),
  (L > 0
   -> L1 is L - 1,
      set_panel_location(Panel, lc(L1, C), Debug0-Debug),
      erase_panel_bottom_line(Debug0, Panel)
   ;  Debug = Debug0).

% move_panel_down(+Debug0--Debug)
move_panel_down(Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_location(Debug0, Panel, lc(L, C)),
  L1 is L + 1,
  set_panel_location(Panel, lc(L1, C), Debug0-Debug),
  erase_panel_upper_line(Debug0, Panel).

% move_panel_left(+Debug0--Debug)
move_panel_left(Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_location(Debug0, Panel, lc(L, C)),
  (C > 0
   -> C1 is C - 1,
      set_panel_location(Panel, lc(L, C1), Debug0-Debug),
      erase_panel_right_column(Debug0, Panel)
   ;  Debug = Debug0).

% move_panel_right(+Debug0--Debug)
move_panel_right(Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_location(Debug0, Panel, lc(L, C)),
  C1 is C + 1,
  set_panel_location(Panel, lc(L, C1), Debug0-Debug),
  erase_panel_left_column(Debug0, Panel).


% resize_panel_enlarge_vertical(+Comp0--Comp, +Debug0--Debug)
resize_panel_enlarge_vertical(Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, control),
  !.
resize_panel_enlarge_vertical(Comp0-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_size(Debug0, Panel, rc(R, C)),
  R1 is R + 1,
  set_panel_size(Panel, rc(R1, C), Debug0-Debug1),
  update_panel_contents(Comp0-Comp, Debug1-Debug).

% resize_panel_shorten_vertical(+Comp0--Comp, +Debug0--Debug)
resize_panel_shorten_vertical(Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, control),
  !.
resize_panel_shorten_vertical(Comp0-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_size(Debug0, Panel, rc(R, C)),
  (R > 1
   -> R1 is R - 1,
      set_panel_size(Panel, rc(R1, C), Debug0-Debug1),
      erase_panel_bottom_line(Debug0, Panel),
      update_panel_contents(Comp0-Comp, Debug1-Debug)
   ;  Comp = Comp0,
      Debug = Debug0).

% resize_panel_enlarge_horizontal(+Comp0--Comp, +Debug0--Debug)
resize_panel_enlarge_horizontal(Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, control),
  !.
resize_panel_enlarge_horizontal(Comp0-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_size(Debug0, Panel, rc(R, C)),
  C1 is C + 1,
  set_panel_size(Panel, rc(R, C1), Debug0-Debug1),
  update_panel_contents(Comp0-Comp, Debug1-Debug).

% resize_panel_shorten_horizontal(+Comp0--Comp, +Debug0--Debug)
resize_panel_shorten_horizontal(Comp-Comp, Debug-Debug) :-
  get_in_focus(Debug, control),
  !.
resize_panel_shorten_horizontal(Comp0-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, Panel),
  get_panel_size(Debug0, Panel, rc(R, C)),
  atom_length(Panel, L),
  L1 is L + 6,
  (C > L1
   -> C1 is C - 1,
      set_panel_size(Panel, rc(R, C1), Debug0-Debug1),
      erase_panel_right_column(Debug0, Panel),
      update_panel_contents(Comp0-Comp, Debug1-Debug)
   ;  Comp = Comp0,
      Debug = Debug0).


% update_panel_contents(+Comp0--Comp, +Debug0--Debug)
update_panel_contents(Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, inspect),
  !,
  debug:update_inspect_listing(Comp, Debug0-Debug).
update_panel_contents(Comp-Comp, Debug0-Debug) :-
  get_in_focus(Debug0, program),
  !,
  update_program_listing(Comp, Debug0-Debug).
update_panel_contents(Comp0-Comp, Debug-Debug) :-
  get_in_focus(Debug, screen),
  !,
  update_screen(Comp0-Comp).
update_panel_contents(Comp-Comp, Debug-Debug) :-
  !.

% update_screen(+Comp0--Comp)
update_screen(Comp0-Comp) :-
  screen:init_screen(Screen, Cursor),
  set_screen(Screen, Comp0-Comp1),
  set_cursor(Cursor, Comp1-Comp).

% erase_panel_bottom_line(+Debug, +Panel)
erase_panel_bottom_line(Debug, Panel) :-
  get_panel_location(Debug, Panel, lc(L, C)),
  get_panel_size(Debug, Panel, rc(R, SC)),
  T is SC + 2,
  blank_row(T, BlankRow),
  L1 is L + R + 1,
  write_at(L1, C, BlankRow).
  
% erase_panel_upper_line(+Debug, +Panel)
erase_panel_upper_line(Debug, Panel) :-
  get_panel_location(Debug, Panel, lc(L, C)),
  get_panel_size(Debug, Panel, rc(_R, SC)),
  T is SC + 2,
  blank_row(T, BlankRow),
  write_at(L, C, BlankRow).

% erase_panel_right_column(+Debug, +Panel)
erase_panel_right_column(Debug, Panel) :-
  get_panel_location(Debug, Panel, lc(L, C)),
  get_panel_size(Debug, Panel, rc(R, SC)),
  C1 is C + SC + 1,
  T is R + 2,
  write_blank_column_at(L, C1, T).
  
% erase_panel_left_column(+Debug, +Panel)
erase_panel_left_column(Debug, Panel) :-
  get_panel_location(Debug, Panel, lc(L, C)),
  get_panel_size(Debug, Panel, rc(R, _SC)),
  T is R + 2,
  write_blank_column_at(L, C, T).
  
% write_blank_column_at(+Line, +Column, +Total)
write_blank_column_at(_L, _C, 0) :-
  !.
write_blank_column_at(L, C, T0) :-
  write_at(L, C, ' '),
  L1 is L + 1,
  T is T0 - 1,
  write_blank_column_at(L1, C, T).


% get_panel_location(+Debug, +Panel, -Location)
get_panel_location(Debug, program, Location) :-
  !,
  get_program_panel_location(Debug, Location).
get_panel_location(Debug, control, Location) :-
  !,
  get_control_panel_location(Debug, Location).
get_panel_location(Debug, inspect, Location) :-
  !,
  get_inspect_panel_location(Debug, Location).
get_panel_location(Debug, screen, Location) :-
  !,
  get_screen_panel_location(Debug, Location).

% set_panel_location(+Panel, -Location, +Debug0--Debug)
set_panel_location(program, Location, Debug0-Debug) :-
  !,
  set_program_panel_location(Location, Debug0-Debug).
set_panel_location(control, Location, Debug0-Debug) :-
  !,
  set_control_panel_location(Location, Debug0-Debug).
set_panel_location(inspect, Location, Debug0-Debug) :-
  !,
  set_inspect_panel_location(Location, Debug0-Debug).
set_panel_location(screen, Location, Debug0-Debug) :-
  !,
  set_screen_panel_location(Location, Debug0-Debug).

% get_panel_size(+Debug, +Panel, -Size)
get_panel_size(Debug, program, Size) :-
  !,
  get_program_panel_size(Debug, Size).
get_panel_size(Debug, control, Size) :-
  !,
  get_control_panel_size(Debug, Size).
get_panel_size(Debug, inspect, Size) :-
  !,
  get_inspect_panel_size(Debug, Size).
get_panel_size(Debug, screen, Size) :-
  !,
  get_screen_panel_size(Debug, Size).

% set_panel_size(+Panel, -Size, +Debug0--Debug)
set_panel_size(program, Size, Debug0-Debug) :-
  !,
  set_program_panel_size(Size, Debug0-Debug).
set_panel_size(control, Size, Debug0-Debug) :-
  !,
  set_control_panel_size(Size, Debug0-Debug).
set_panel_size(inspect, Size, Debug0-Debug) :-
  !,
  set_inspect_panel_size(Size, Debug0-Debug).
set_panel_size(screen, Size, Debug0-Debug) :-
  !,
  set_screen_panel_size(Size, Debug0-Debug).
