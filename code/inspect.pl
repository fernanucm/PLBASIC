/*********************************************************/
/*                                                       */
/* INSPECT module for the BASIC language                 */
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

:- module(inspect,
          [ inspect_listing/4,
            display_inspects/1
          ]).

:- use_module(misc,
          [ blank_rows/3,
            display_frame/5,
            panel_huframe_with_title/4,
            panel_hlframe/2,
            take_n_at/4,
            split_at/4
          ]).

:- use_module(comp_obj,
          [ get_mem/3
          ]).

:- use_module(debug_obj,
          [ get_inspect_panel_location/2,
            get_inspect_panel_size/2,
            get_inspect_display_offset/2,
            get_inspect_listing/2,
            in_focus/3
          ]).

:- use_module(list,
          [ format_expr/3 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for inspect listing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inspect_listing(+Comp, +InspectPanelSize, +InspectVars, -InspectListing)
inspect_listing(Comp, rc(NbrRows, NbrColumns), InspectVars, InspectListing) :-
  VarWidth = 8,
  list_inspects_to_listing(InspectVars, Comp, NbrRows, NbrColumns, VarWidth, 0, InspectListing-_).

% list_inspects_to_listing(+Vars, +Comp, +Height, +Width, +VarWidth, +RowNbr, -IL0-IL)
list_inspects_to_listing([], _Comp, Height, Width, _VarWidth, _RowNbr, IL-IL) :-
  !,
  blank_rows(Height, Width, IL).
list_inspects_to_listing([Var|Vars], Comp, Height, Width, VarWidth, RowNbr, IL0-IL) :-
  (get_mem(Comp, id(Var), Expr) -> true ; Expr = str('<null>')),
  format_expr(id(Var), VarCodes, []),
  format_expr(Expr, ExprCodes, []),
  format(codes(VarValCodes), '~|~` t~s~*+: ~s', [VarCodes, VarWidth, ExprCodes]),
  format_var_value_in_width(VarValCodes, Width, true, FVarValueList),
  length(FVarValueList, NbrRows),
  append(FVarValueList, IL1, IL0),
  RowNbr1 is RowNbr + NbrRows,
  list_inspects_to_listing(Vars, Comp, Height, Width, VarWidth, RowNbr1, IL1-IL).

% format_var_value_in_width(+Codes, +Width, +IsFirst, -FVarValueList)
format_var_value_in_width([], _Width, _IsFirst, []) :-
  !.
format_var_value_in_width(Codes0, Width, IsFirst, [F|Fs]) :-
  split_at(Width, Codes0, Codes1, RemCodes),
  atom_codes(VarVal, Codes1),
  (IsFirst
   -> format(atom(F), '~w', [VarVal])
   ;  format(atom(F), '~|~` t~*+ ~w', [Width, VarVal])),
  format_var_value_in_width(RemCodes, Width, false, Fs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display inspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_inspects(+Debug) :-
display_inspects(Debug) :-
  get_inspect_panel_location(Debug, Location),
  get_inspect_panel_size(Debug, rc(TotalRows, Columns)),
  get_inspect_display_offset(Debug, DisplayOffset),
  Width is Columns + 2,
  in_focus(Debug, inspect, InFocus),
  panel_huframe_with_title('INSPECT', Width, InFocus, UFrame),
  inspect_lines(Debug, DisplayOffset, TotalRows, InspectRows),
  panel_hlframe(Width, LFrame),
  display_frame(Location, TotalRows, UFrame, InspectRows, LFrame).

% inspect_lines(+Debug, +Offset, +TotalRows, -Rows) is det
inspect_lines(Debug, Offset, TotalRows, Rows) :-
  get_inspect_listing(Debug, Listing),
  StartRowNbr is max(-Offset, 0),
  take_n_at(Listing, TotalRows, StartRowNbr, Rows).


