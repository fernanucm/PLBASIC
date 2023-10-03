/*********************************************************/
/*                                                       */
/* FILE module for the BASIC language                    */
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

:- module(color,
          [ text_color/2,
            color/3,
            apply_text_color/3 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEXT Defined Colors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% With ANSI escape codes,
% swipl-win.exe supports both normal and bright colors
% swipl.exe supports only normal colors
% See misc.pl for color names

% Color fb(Foreground, Backgroud)
% text_color(interface, fb(white, bright_blue)). % Not used yet. See themes
text_color(focused,     fb(bright_blue, white)).
text_color(heading,     fb(bright_white, bright_black)).
text_color(highlighted, fb(bright_blue, white)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANSI Colors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ANSI Colors as defined for 3-bit and 4-bit
% https://en.wikipedia.org/wiki/ANSI_escape_code
% Normal foreground colors are preceded by 3 while
% normal background colors are preceded by 4.
% Bright foreground colors are preceded by 9 while
% bright background colors are preceded by 10.

color(foreground, black,          30).
color(foreground, red,            31).
color(foreground, green,          32).
color(foreground, yellow,         33).
color(foreground, blue,           34).
color(foreground, magenta,        35).
color(foreground, cyan,           36).
color(foreground, white,          37).
color(background, black,          40).
color(background, red,            41).
color(background, green,          42).
color(background, yellow,         43).
color(background, blue,           44).
color(background, magenta,        45).
color(background, cyan,           46).
color(background, white,          47).
color(foreground, bright_black,   90).
color(foreground, bright_red,     91).
color(foreground, bright_green,   92).
color(foreground, bright_yellow,  93).
color(foreground, bright_blue,    94).
color(foreground, bright_magenta, 95).
color(foreground, bright_cyan,    96).
color(foreground, bright_white,   97).
color(background, bright_black,  100).
color(background, bright_red,    101).
color(background, bright_green,  102).
color(background, bright_yellow, 103).
color(background, bright_blue,   104).
color(background, bright_magenta,105).
color(background, bright_cyan,   106).
color(background, bright_white,  107).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COLOR Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% apply_text_color(+Text0, +Color, -Text)
apply_text_color(Text0, Color, Text) :-
  text_color(Color, fb(FGColor, BGColor)),
  color(foreground, FGColor, FGCode),
  color(background, BGColor, BGCode),
  format(atom(Text), '\33[~d;~dm~w\33[0m', [FGCode, BGCode, Text0]).

