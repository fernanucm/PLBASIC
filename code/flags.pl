/*********************************************************/
/*                                                       */
/* FLAGS for the BASIC system                            */
/*    Version Seiko Data 2000                            */
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

:- module(flags,
          [ set_flag/1,
            uppercase/1,
            let/1,
            optional_spaces/1,
            line_number_width/1,
            screen_panel_location/1,
            screen_panel_size/1,
            running_screen/1,
            tab_width/1,
            program_panel_location/1,
            program_panel_size/1,
            inspect_panel_location/1,
            inspect_panel_size/1,
            control_panel_location/1,
            control_panel_size/1,
            focus_order/1,
            file_panel_size/1,
            file_panel_location/1,
            current_open_folder/1 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set_flag(+Flag) is det
% Set the flag with name Name to value Value
set_flag(NewFlag) :-
  NewFlag  =.. [Name, _],
  OpenFlag =.. [Name, _],
  (\+ clause(flags:OpenFlag, true)
   -> format('Error: Unknown flag ~w', [NewFlag])
   ;  retractall(OpenFlag),
      assertz(NewFlag)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Uppercase: listings are provided in either uppercase (on) or lowercase (off)
:- dynamic uppercase/1.
uppercase(on).

% Let: listings includes LET (on) or not (off)
:- dynamic let/1.
let(off).

% Optional spaces: listings includes optional spaces (on) or not (off)
:- dynamic optional_spaces/1.
optional_spaces(on).

% line_number_width(-Width)
% 00000:00
line_number_width(8).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCREEN Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% screen_panel_location(lc(-LineNbr, -ColumnNbr)).
% Include the frame of 1 character-width
:- dynamic screen_panel_location/1.
% screen_panel_location(lc(0, 43)).
screen_panel_location(lc(3, 50)).

% screen_panel_size(rc(-NbrLines, -NbrColumns)).
:- dynamic screen_panel_size/1.
% screen_panel_size(rc(11, 27)).
screen_panel_size(rc(5, 10)).

% running_screen(?Switch)
% Enables screen display for each executed statement
:- dynamic running_screen/1.
running_screen(off).

% tab_width(-Tab)
% Number of columns between tabulators
% 8 is the default tab width for Seiko Data 2000
tab_width(8).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROGRAM Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% program_panel_location(lc(-LineNbr, -ColumnNbr)).
% Include the frame of 1 character-width
:- dynamic program_panel_location/1.
program_panel_location(lc(0, 0)).

% program_panel_size(rc(-NbrLines, -NbrColumns)).
:- dynamic program_panel_size/1.
program_panel_size(rc(11, 40)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSPECT Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inspect_panel_location(lc(-LineNbr, -ColumnNbr)).
% Include the frame of 1 character-width
:- dynamic inspect_panel_location/1.
%inspect_panel_location(11, 43).
inspect_panel_location(lc(13, 30)).

% inspect_panel_size(rc(-NbrLines, -NbrColumns)).
:- dynamic inspect_panel_size/1.
inspect_panel_size(rc(11, 40)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTROL Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% control_panel_location(lc(-LineNbr, -ColumnNbr)).
% Include the frame of 1 character-width
:- dynamic control_panel_location/1.
control_panel_location(lc(13, 0)).

% control_panel_size(rc(-NbrLines, -NbrColumns)).
:- dynamic control_panel_size/1.
control_panel_size(rc(11, 27)).

% focus_order(-List)
% Panel focus order when pressing TAB
focus_order([program, inspect, screen, control]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% File panel width is automatically adjusted when 
%   filling the file panel
:- dynamic file_panel_size/1.
file_panel_size(rc(15,_)).

:- dynamic file_panel_location/1.
file_panel_location(lc(0,0)).

:- dynamic current_open_folder/1.
current_open_folder('./bas').

