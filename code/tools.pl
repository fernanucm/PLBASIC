/*********************************************************/
/*                                                       */
/* TOOLS for the BASIC language                          */
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

:- module(tools,
          [ renum_source_comp/4,
            output_program/1 ]).

:- use_module(library(assoc)).

:- use_module(basic,
          [ run/2 ]).

:- use_module(comp_obj,
          [ get_line/2,
            get_runline/2,
            get_program/2,
            get_data/2,
            get_program_start_line/2,
            get_statement_and_next_line/4,
            empty_program/1,
            set_line/2,
            set_runline/2,
            set_data/2,
            set_program/2 ]).

:- use_module(list,
          [ listing_to_print_commands/3 ]).

:- use_module(misc,
          [ ceiling_line/3 ]).

:- use_module(test,
          [ test/4 ]).

:- use_module(error_,
          [ set_error/3,
            process_error/0 ]).

% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Renumbering (renum)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% renum_source_comp(+Source, +Start, +Step, -Comp) is det
%
% Renumber source (either codes or a file) 
renum_source_comp(Source, Start, Step, Comp) :-
  basic:lex_parse(Source, StatementList), % Do not transform
  comp_obj:init_comp(Source, StatementList, Comp0),
  renum(Start, Step, Comp0-Comp1),
  comp_obj:get_program_statements(Comp1, RStatementList), 
  parser:transform(RStatementList, TStatementList), 
  comp_obj:init_comp(Source, TStatementList, Comp).

%% renum_source(+Source, +Start, +Step) is det
%
% Renumber source (either codes or a file) and 
%   output the result
renum_source(Source, Start, Step) :-
  % Renumber
  renum_source_comp(Source, Start, Step, Comp),
  % List the result
  get_program(Comp, Program),
  get_program_start_line(Program, Label),
  listing_to_print_commands(Label, Comp, Commands),
  output_commands(Commands).

%% renum(+Start, +Step, +Comp0--Comp) is det
%
% Renumber lines from the program in Comp0 starting at number 
% Start with a step Step, giving the result in Comp
% This tool must be applied before any program transformations
% (as transform_ifs after syntax parsing), 
% just after the parsing itself

renum(Start, Step, Comp0-Comp) :-
  get_program(Comp0, Program),
  get_program_start_line(Program, Line),
  empty_assoc(Dict0),
  Start1 is Start - Step,
  build_dict(Comp0, void, Line, Start1, Step, EndLine, Dict0-Dict),
  empty_program(Program0),
  renum_program(Comp0, Dict, Line, Step, EndLine, Program0-RProgram),
  set_comp_lines(Dict, Comp0-Comp1),
  set_program(RProgram, Comp1-Comp).

%% set_comp_lines(+Dict, +Comp0--Comp) is det
set_comp_lines(Dict, Comp0-Comp) :-
  get_line(Comp0, Line),
  get_runline(Comp0, RunLine),
  get_data(Comp0, DataLine),
  get_assoc(Line, Dict, RLine),
  get_assoc(RunLine, Dict, RRunLine),
  translate_data_line(DataLine, Dict, RDataLine),
  set_line(RLine, Comp0-Comp1),
  set_runline(RRunLine, Comp1-Comp2),
  set_data(RDataLine, Comp2-Comp).

% translate_data_line(+DataLine, +Dict, -RDataLine) 
translate_data_line(void, _Dict, void) :-
  !.
translate_data_line(DataLine-DataOrder, Dict, RDataLine-DataOrder) :-
  get_assoc(DataLine, Dict, RDataLine).

%% build_dict(+Comp, +LastLine, +Line, +Start, +Step, -EndLine, +Dict0--Dict) is det
%
% Build a dictionary (assoc list) of key-value for original line 
%  number and renumbered line number, respectively.
% build_dict(_Comp, _, void-void, _Start, _Step, EndLine, Dict0-Dict) :-
%   put_assoc(void-void, Dict0, void-void, Dict),
%   !.
build_dict(Comp, Last, Line-Stmt, Start, Step, EndLine, Dict0-Dict) :-
  get_statement_and_next_line(Comp, Line-Stmt, Statement, NextLine),
  (Line \== Last
   -> NewStart is Start + Step
   ;  NewStart = Start),
  put_assoc(Line-Stmt, Dict0, NewStart-Stmt, Dict1),
  add_to_dict_if_branches(NewStart, Statement, Dict1-Dict2),
  (NextLine == void-void
   -> EndLine = Line,
      put_assoc(void-void, Dict2, void-void, Dict)
   ;  build_dict(Comp, Line, NextLine, NewStart, Step, EndLine, Dict2-Dict)).

%% add_to_dict_if_branches(+Start, +Statement, +Dict0--Dict) is det
%
add_to_dict_if_branches(Start, if(_Cond, Then, Else, _Source), Dict0-Dict) :-
  !,
  add_to_dict_branches(Then, Start, Dict0-Dict1),
  add_to_dict_branches(Else, Start, Dict1-Dict).
add_to_dict_if_branches(_, _, Dict-Dict).

%% add_to_dict_branches(+Then, +Start, +Dict0--Dict1) is det
%
add_to_dict_branches([], _Start, Dict-Dict) :-
  !.
add_to_dict_branches([Line-Stmt:_Statement|Statements], Start, Dict0-Dict) :-
  put_assoc(Line-Stmt, Dict0, Start-Stmt, Dict1),
  add_to_dict_branches(Statements, Start, Dict1-Dict).

%% renum_program(+Comp, +Dict, +Line, +Program0--Program) is det
%
% Renumber the program in Comp starting at line Line,
%   using the dictionary Dict of line addresses and
%   returning the renumerated program in Program.
% Program0 is the accumulated resulting program
renum_program(_Comp, _Dict, void-void, _Step, _EndLine, Program-Program) :-
  !.
renum_program(Comp, Dict, Line, Step, EndLine, Program0-Program) :-
  get_statement_and_next_line(Comp, Line, Statement, NextLine),
  get_assoc(Line, Dict, RLine),
  get_assoc(NextLine, Dict, RNextLine),
  renum_statement(Statement, Comp, Dict, Step, EndLine, RStatement),
  put_assoc(RLine, Program0, RStatement:RNextLine, Program1),
  renum_program(Comp, Dict, NextLine, Step, EndLine, Program1-Program).


%% renum_statement(+Statement, +Comp, +Dict, +Step, +EndLine, -RStatement) is det
%
%  Renumber a statement
renum_statement(data(Data, void), _Comp, _Dict, _Step, _EndLine, data(Data, void)) :-
  !.
renum_statement(data(Data, Label), Comp, Dict, Step, EndLine, data(Data, RLabel)) :-
  !,
  renum_label(Label, Comp, Dict, Step, EndLine, RLabel).
renum_statement(restore(Line, Prov), Comp, Dict, Step, EndLine, restore(RLine, Prov)) :-
  !,
  renum_label_exp(Line-int(1), Comp, Dict, Step, EndLine, RLine-_).
renum_statement(goto(Line), Comp, Dict, Step, EndLine, goto(RLine)) :-
  !,
  renum_label_exp(Line, Comp, Dict, Step, EndLine, RLine).
renum_statement(goto(Expr, Lines), Comp, Dict, Step, EndLine, goto(Expr, RLines)) :-
  !,
  renum_label_exps(Dict, Comp, Lines, Step, EndLine, RLines).
renum_statement(gosub(Line), Comp, Dict, Step, EndLine, gosub(RLine)) :-
  !,
  renum_label_exp(Line, Comp, Dict, Step, EndLine, RLine).
renum_statement(gosub(Expr, Lines), Comp, Dict, Step, EndLine, gosub(Expr, RLines)) :-
  !,
  renum_label_exps(Dict, Comp, Lines, Step, EndLine, RLines).
renum_statement(run(Line, Prov), Comp, Dict, Step, EndLine, run(RLine, Prov)) :-
  !,
  renum_label_exp(Line, Comp, Dict, Step, EndLine, RLine).
renum_statement(list(Line, Prov), Comp, Dict, Step, EndLine, list(RLine, Prov)) :-
  !,
  renum_label_exp(Line, Comp, Dict, Step, EndLine, RLine).
renum_statement(if(Cond, ThenList, ElseList, source(ThenList, ElseList)), Comp, Dict, Step, EndLine, if(Cond, RThenList, RElseList, source(RThenList, RElseList))) :-
  !,
  renum_lines(ThenList, Comp, Dict, Step, EndLine, RThenList),
  renum_lines(ElseList, Comp, Dict, Step, EndLine, RElseList).
renum_statement(restore(Line, Prov), Comp, Dict, Step, EndLine, restore(RLine, Prov)) :-
  !,
  renum_label_exp(Line, Comp, Dict, Step, EndLine, RLine).
renum_statement(Statement, _Comp, _Dict, _Step, _EndLine, Statement).

%% renum_label_exps(Lines, Comp, Dict, Step, EndLine, RLines)
%
renum_label_exps([], _Comp, _Dict, _Step, _EndLine, []) :-
  !.
renum_label_exps([LabelExp|LabelExps], Comp, Dict, Step, EndLine, [RLabelExp|RLabelExps]) :-
  renum_label_exp(LabelExp, Comp, Dict, Step, EndLine, RLabelExp),
  renum_label_exps(LabelExps, Comp, Dict, Step, EndLine, RLabelExps).


%% renum_lines(+Lines, +Comp, +Dict, +Step, +EndLine, +Step, -Statements)
%
% Renum the list of Lines (Label:Statement)
renum_lines([], _Comp, _Dict, _Step, _EndLine, []).
renum_lines([Label:Statement|Lines], Comp, Dict, Step, EndLine, [RLabel:RStatement|RStatements]) :-
  renum_label(Label, Comp, Dict, Step, EndLine, RLabel),
  renum_statement(Statement, Comp, Dict, Step, EndLine, RStatement),
  renum_lines(Lines, Comp, Dict, Step, EndLine, RStatements).

%% renum_label_exp(+LabelExp, +Comp, +Dict, +Step, +EndLine, +Step, -RLine)
%
% Look the key for the ceiling of Line in Dict and
%   return the value RLine
renum_label_exp(int(L)-int(S), Comp, Dict, Step, EndLine, int(RL)-int(RS)) :-
  !,
  renum_label(L-S, Comp, Dict, Step, EndLine, RL-RS).
% Don't know what to do with expressions yet
renum_label_exp(LabelExp, _Comp, _Dict, _Step, _EndLine, LabelExp).

renum_label(L-S, Comp, Dict, Step, EndLine, RL-RS) :-
  ceiling_line(Comp, L-S, Label),
  (Label == void-void
   -> get_assoc(EndLine-1, Dict, REndLine-1),
      RL is REndLine + Step, 
      RS = 1
   ;  get_assoc(L-S, Dict, RL-RS)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output program (output_program)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% output_program(+Comp) is det
%
% Output to the current output stream the listing of the
%   program in Comp

output_program(Comp) :-
  get_program(Comp, Program),
  get_program_start_line(Program, Start),
  listing_to_print_commands(Start, Comp, Commands),
  output_commands(Commands).

output_commands([]).
output_commands([nl|Commands]) :-
  format('\n', []),
  output_commands(Commands).
output_commands([val(String)|Commands]) :-
  format('~w', [String]),
  output_commands(Commands).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(tools).

% Set of tests
% To test all of them: 
%   ?- tools:test.
% Only a couple. Better run tests at main


% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.
test_renum(renum(Source, Start, Step), Comp) :-
  renum_source_comp(Source, Start, Step, Comp0), 
  interpreter:interpret_statement(list(int(1), false), Comp0-Comp), 
  screen:display_screen(Comp).  

test_list(Source, Listing) :-
  run(Source, Comp),
  with_output_to_codes(output_program(Comp), Codes),
  atom_codes(Listing, Codes).

test_renum_list(Source, Listing) :-
  with_output_to_codes(renum_source(Source, 10, 10), Codes),
  atom_codes(Listing, Codes).

test001 :-
  screen:set_screen_panel_size(4,10),
  Source = "10 print 1",
  Result = comp{cursor:lc(1,0),data:void,line:1-1,mem:t,program:t(1-1,print([int(1)]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'1 PRINT 1                               ',source:[49,48,32,112,114,105,110,116,32,49],stack:[],status:run},
  test(tools, test_renum, renum(Source, 1, 1), Result).

test002 :-
  screen:set_screen_panel_size(4,10),
  Source = "10 goto 30\n20 goto 40\n30 goto 10",
  Result = comp{cursor:lc(3,0),data:void,line:1-1,mem:t,program:t(3-1,goto(int(1)-int(1)):void-void,<,t(2-1,goto(int(4)-int(1)):3-1,<,t(1-1,goto(int(3)-int(1)):2-1,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'1 GOTO 3  2 GOTO 4  3 GOTO 1            ',source:[49,48,32,103,111,116,111,32,51,48,10,50,48,32,103,111,116,111,32,52,48,10,51,48,32,103,111,116,111,32,49,48],stack:[],status:run},
  test(tools, test_renum, renum(Source, 1, 1), Result).

test003 :-
  screen:set_screen_panel_size(3,40),
  Program = "10 if 1=1 then goto 20 else goto 30\n20 print 1",
  Result = comp{cursor:lc(2,0),data:void,line:1-1,mem:t,program:t(1-3,goto(int(2)-int(1)):1-4,-,t(1-2,goto(int(2)-int(1)):1-3,<,t(1-1,if(int(1)=int(1),skip,goto(int(1)-int(4)),source([1-2:goto(int(2)-int(1))],[1-3:goto(int(3)-int(1))])):1-2,-,t,t),t),t(2-1,print([int(1)]):void-void,-,t(1-4,goto(int(3)-int(1)):2-1,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'1 IF 1 = 1 THEN GOTO 2 ELSE GOTO 3      2 PRINT 1                                                                       ',source:[49,48,32,105,102,32,49,61,49,32,116,104,101,110,32,103,111,116,111,32,50,48,32,101,108,115,101,32,103,111,116,111,32,51,48,10,50,48,32,112,114,105,110,116,32,49],stack:[],status:run},
  test(tools, test_renum, renum(Program, 1, 1), Result).

test004 :-
  screen:set_screen_panel_size(3,51),
  Program = "10 if 1=1 then print 1:goto 20 else print 2:goto 30\n20 print 1",
  Result = comp{cursor:lc(2,0),data:void,line:1-1,mem:t,program:t(1-5,print([int(2)]):1-6,<,t(1-3,goto(int(2)-int(1)):1-4,<,t(1-2,print([int(1)]):1-3,<,t(1-1,if(int(1)=int(1),skip,goto(int(1)-int(5)),source([1-2:print([int(1)]),1-3:goto(int(2)-int(1))],[1-4:print([int(2)]),1-5:goto(int(3)-int(1))])):1-2,-,t,t),t),t(1-4,goto(int(2)-int(1)):1-5,-,t,t)),t(2-1,print([int(1)]):void-void,-,t(1-6,goto(int(3)-int(1)):2-1,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'1 IF 1 = 1 THEN PRINT 1:GOTO 2 ELSE PRINT 2:GOTO 3 2 PRINT 1                                                                                             ',source:[49,48,32,105,102,32,49,61,49,32,116,104,101,110,32,112,114,105,110,116,32,49,58,103,111,116,111,32,50,48,32,101,108,115,101,32,112,114,105,110,116,32,50,58,103,111,116,111,32,51,48,10,50,48,32,112,114,105,110,116,32,49],stack:[],status:run},
  test(tools, test_renum, renum(Program, 1, 1), Result).

% Test for listing
test005 :-
  screen:set_screen_panel_size(4,10),
  Program = 'test/ball.bas',
  Result = '1 DEF FNMOD10(X) = (X / 10 - INT(X / 10)) * 10\n2 DEF FNMOD4(X) = (X / 4 - INT(X / 4)) * 4\n3 L = INT(RND(1) * 4)\n4 C = INT(RND(1) * 10)\n5 DL = INT(RND(1) * 2 - 1)\n6 DC = INT(RND(1) * 2 - 1)\n7 FOR I = 1 TO 10\n8 REM print l;" ";c\n9 LOCATE L, C\n10 PRINT " ";\n11 C = INT(ABS(FNMOD10(C + DC)))\n12 L = INT(ABS(FNMOD4(L + DL)))\n13 LOCATE L, C\n14 PRINT "*";\n15 PAUSE 0.2\n16 NEXT I\n',
  misc:cls,
  test(tools, test_list, Program, Result).

test006 :-
  screen:set_screen_panel_size(3,51),
  Program = 'test/ball.bas',
  Result = '10 DEF FNMOD10(X) = (X / 10 - INT(X / 10)) * 10\n20 DEF FNMOD4(X) = (X / 4 - INT(X / 4)) * 4\n30 L = INT(RND(1) * 4)\n40 C = INT(RND(1) * 10)\n50 DL = INT(RND(1) * 2 - 1)\n60 DC = INT(RND(1) * 2 - 1)\n70 FOR I = 1 TO 10\n80 REM print l;" ";c\n90 LOCATE L, C\n100 PRINT " ";\n110 C = INT(ABS(FNMOD10(C + DC)))\n120 L = INT(ABS(FNMOD4(L + DL)))\n130 LOCATE L, C\n140 PRINT "*";\n150 PAUSE 0.2\n160 NEXT I\n',
  test(tools, test_renum_list, Program, Result).


