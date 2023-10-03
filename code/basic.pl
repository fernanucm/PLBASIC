/*********************************************************/
/*                                                       */
/* An interpreter for the BASIC language                 */
/*    Version Seiko Data 2000                            */
/*                                                       */
/*   Composed of:                                        */
/*     - Lexer                                           */
/*     - Parser                                          */
/*     - Interpreter, based on:                          */
/*       Version 0.1 of PROLOG BASIC.                    */
/*       Author: Victor Lagerkvist                       */
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

:- module(basic,
          [ run/1,
            run/2,
            lex_parse/2,
            lex_parse_transform/2,
            cls/0 ]).

%:- use_module('themes/dark_green.pl').
:- use_module('themes/blue_white.pl').
:- use_module(flags,
          [ set_flag/1,
            running_screen/1 ]).
:- use_module(error_,
          [ set_error/3,
            reset_error/0,
            process_error/0 ]).
:- use_module(lexer).
:- use_module(parser).
:- use_module(interpreter).
:- use_module(tools).
:- use_module(misc,
          [ cls/0,
            home/0 ]).
:- use_module(comp_obj,
          [ set/4,
            set_last_void_statement/2,
            label_to_data_address/2,
            init_comp/2,
            init_comp/3,
            get_statement_and_update_lines/2,
            get_status/2,
            set_status/2 ]).
:- use_module(screen).
:- use_module(test,
          [ test/4 ]).


% This SWI-Prolog flag makes strings delimited by double
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).

% Running tests:
% ?- basic:test.


% run(+Input, -Comp)
% Main predicate of this module
% Run the given input (either a filename or list of codes describing a
%  program) and return the comp object as described in comp.pl
% Running includes lexical and syntactical analysis followed
% by interpreting the input program
run(Input, Comp) :-
  set_flag(screen_panel_location(lc(0, 0))),
  init_comp(Input, Comp0),
  interpret_statements(Comp0-Comp).

% run(+Input)
% Same as run/2, without returning the comp object
run(Input) :-
  run(Input, _Comp).

% lex_parse(+Input, -SyntaxTree)
% Lexical and syntax analysis
% Tools such as renum can be used here
lex_parse(Input, SyntaxTree) :-
  lex(Input, Tokens),
  parse(Tokens, SyntaxTree).

% lex_parse_transform(+Input, -SyntaxTree)
% Lexical and syntax analysis, followed by transform
% This is the predicate used before running a program
lex_parse_transform(Input, SyntaxTree) :-
  lex(Input, Tokens),
  parse_transform(Tokens, SyntaxTree).

% interpret_statements(+Comp0--Comp).
% Interpret the program in Comp0 and
% return the resulting comp object
interpret_statements(Comp0-Comp) :-
  reset_error,
  home,
  interpret(run, Comp0-Comp),
  % display_screen(Comp),
  !.
interpret_statements(_Comp0_Comp) :-
  process_error,
  !, fail.

% interpret(+Status, +Comp0--Comp)
% Interpret the current statement as long as the status is 'run' (running).
interpret(run, Comp0-Comp) :-
  !,
  get_statement_and_update_lines(S, Comp0-Comp1),
  home,
  interpret_statement(S, Comp1-Comp2),
  get_status(Comp2, Status),
  % (running_screen(on), Status == run -> display_screen(Comp2) ; true),
  display_screen(Comp2),
  % sleep(0.2),
  interpret(Status, Comp2-Comp).
interpret(stop, Comp0-Comp) :-
  !,
  interpret_statement(stop, Comp0-Comp).
interpret(end, Comp0-Comp) :-
  !,
  interpret_statement(end, Comp0-Comp).
interpret(new, Comp0-Comp) :-
  interpret_statement(end, Comp0-Comp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(basic).

% Set of tests
% To test all of them:
%   ?- basic:test.

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.
test001 :-
  screen:set_screen_panel_size(5,10),
  test(basic, lex_parse, " ", []).

test002 :-
  test(basic, lex_parse, "\n \n", []).

test003 :-
  test(basic, lex_parse, "10 x=tan(2)^1.1", [10-1:let(id(x),fn(tan(int(2)))^frac(1,1))]).

test004 :-
  test(basic, lex_parse, "\n10 let x=sin+and+tan(2)^1.1", [10-1:let(id(x),id(sin)+id(and)+fn(tan(int(2)))^frac(1,1))]).

test005 :-
  test(basic, lex_parse, "10 for i=-sin(1) to 2 step tan(2)+1\n", [10-1:for(id(i),-fn(sin(int(1))),int(2),fn(tan(int(2)))+int(1),true)]).

test006 :-
  test(basic, lex_parse, "10 let sin=sin(1)", [10-1:let(id(sin),fn(sin(int(1))))]).

test007 :-
  test(basic, lex_parse, "\n10 let x=1:let z=2\n20 let y=0:u=3", [10-1:let(id(x), int(1)), 10-2:let(id(z), int(2)), 20-1:let(id(y), int(0)), 20-2:let(id(u), int(3))]).

test008 :-
  test(basic, lex_parse, "10 x=1\n20 for i=1 to 10\n30 x=x*i\n40 next i", [10-1:let(id(x),int(1)),20-1:for(id(i),int(1),int(10),int(1),false),30-1:let(id(x),id(x)*id(i)),40-1:next(id(i))]).

test009 :-
  test(basic, lex_parse, "10 print sqr(2)", [10-1:print([fn(sqr(int(2)))])]).

test010 :-
  test(basic, run, "10 let x=1\n20 for i=1 to 4\n30 let x=x*i\n40 next i", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),24,<,t(id(i),5,-,t,t),t),program:t(40-1,next(id(i)):void-void,<,t(20-1,for(id(i),int(1),int(4),int(1),false):30-1,-,t(10-1,let(id(x),int(1)):20-1,-,t,t),t(30-1,let(id(x),id(x)*id(i)):40-1,-,t,t)),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,108,101,116,32,120,61,49,10,50,48,32,102,111,114,32,105,61,49,32,116,111,32,52,10,51,48,32,108,101,116,32,120,61,120,42,105,10,52,48,32,110,101,120,116,32,105],stack:[],status:end}).

test011 :-
  test(basic, run, "10 if 1<0 then x=1", comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-1,if(int(1)<int(0),let(id(x),int(1)),skip,source([10-2:let(id(x),int(1))],[])):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,120,61,49],stack:[],status:end}).

test012 :-
  test(basic, run, "10 if 1>0 then x=1", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),1,-,t,t),program:t(10-1,if(int(1)>int(0),let(id(x),int(1)),skip,source([10-2:let(id(x),int(1))],[])):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,120,61,49],stack:[],status:end}).

test013 :-
  test(basic, run, "10 if 1>0 then 20",  comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-1,if(int(1)>int(0),goto(int(20)-int(1)),skip,source([10-2:goto(int(20)-int(1))],[])):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,50,48],stack:[],status:end}).

test014 :-
  test(basic, run, "10 if 1>0 then x=1:x=2", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),2,-,t,t),program:t(10-3,let(id(x),int(2)):void-void,<,t(10-2,let(id(x),int(1)):10-3,<,t(10-1,if(int(1)>int(0),skip,goto(int(void)-int(void)),source([10-2:let(id(x),int(1)),10-3:let(id(x),int(2))],[])):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,120,61,49,58,120,61,50],stack:[],status:end}).

test015 :-
  test(basic, run, "10 if 1<0 then x=1:x=2",  comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-3,let(id(x),int(2)):void-void,<,t(10-2,let(id(x),int(1)):10-3,<,t(10-1,if(int(1)<int(0),skip,goto(int(void)-int(void)),source([10-2:let(id(x),int(1)),10-3:let(id(x),int(2))],[])):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,120,61,49,58,120,61,50],stack:[],status:end}).

test016 :-
  test(basic, run, "10 if 1<0 then x=1: goto 30\n20 x=2\n30 end", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),2,-,t,t),program:t(10-3,goto(int(30)-int(1)):20-1,-,t(10-2,let(id(x),int(1)):10-3,<,t(10-1,if(int(1)<int(0),skip,goto(int(20)-int(1)),source([10-2:let(id(x),int(1)),10-3:goto(int(30)-int(1))],[])):10-2,-,t,t),t),t(30-1,end:void-void,-,t(20-1,let(id(x),int(2)):30-1,-,t,t),t(void-void,end:void-void,-,t,t))),runline:30-1,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,120,61,49,58,32,103,111,116,111,32,51,48,10,50,48,32,120,61,50,10,51,48,32,101,110,100],stack:[],status:end}).

test017 :-
  test(basic, run, "10 if 1<0 then 30\n20 x=2\n30 end", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),2,-,t,t),program:t(30-1,end:void-void,<,t(20-1,let(id(x),int(2)):30-1,<,t(10-1,if(int(1)<int(0),goto(int(30)-int(1)),skip,source([10-2:goto(int(30)-int(1))],[])):20-1,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:30-1,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,51,48,10,50,48,32,120,61,50,10,51,48,32,101,110,100],stack:[],status:end}).

test018 :-
  test(basic, run, "10 if 1>0 then 30\n20 x=2\n30 end", comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(30-1,end:void-void,<,t(20-1,let(id(x),int(2)):30-1,<,t(10-1,if(int(1)>int(0),goto(int(30)-int(1)),skip,source([10-2:goto(int(30)-int(1))],[])):20-1,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:30-1,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,51,48,10,50,48,32,120,61,50,10,51,48,32,101,110,100],stack:[],status:end}).

test019 :-
  test(basic, run, "10 if 1>0 then 30\n20 x=2", comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(20-1,let(id(x),int(2)):void-void,-,t(10-1,if(int(1)>int(0),goto(int(30)-int(1)),skip,source([10-2:goto(int(30)-int(1))],[])):20-1,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,51,48,10,50,48,32,120,61,50],stack:[],status:end}).

test020 :-
  test(basic, run, "10 if 1<0 then 30\n20 x=2", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),2,-,t,t),program:t(20-1,let(id(x),int(2)):void-void,-,t(10-1,if(int(1)<int(0),goto(int(30)-int(1)),skip,source([10-2:goto(int(30)-int(1))],[])):20-1,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,51,48,10,50,48,32,120,61,50],stack:[],status:end}).

test021 :-
  test(basic, run, "10 if 1<0 then 30:x=1", comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-3,let(id(x),int(1)):void-void,<,t(10-2,goto(int(30)-int(1)):10-3,<,t(10-1,if(int(1)<int(0),skip,goto(int(void)-int(void)),source([10-2:goto(int(30)-int(1)),10-3:let(id(x),int(1))],[])):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,51,48,58,120,61,49],stack:[],status:end}).

test022 :-
  test(basic, run, "10 if 1>0 then 30:x=1", comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-3,let(id(x),int(1)):void-void,<,t(10-2,goto(int(30)-int(1)):10-3,<,t(10-1,if(int(1)>int(0),skip,goto(int(void)-int(void)),source([10-2:goto(int(30)-int(1)),10-3:let(id(x),int(1))],[])):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,51,48,58,120,61,49],stack:[],status:end}).

test023 :-
  test(basic, run, "10 if 1>0 then x=1:30", failure(error('Syntax', 'Statement', pos(1, 20)))).

test024 :-
  test(basic, run, "10 if 1>0 then x=1:y=2 else x=2:y=3", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),1,>,t,t(id(y),2,-,t,t)),program:t(10-4,goto(int(void)-int(void)):10-5,-,t(10-2,let(id(x),int(1)):10-3,-,t(10-1,if(int(1)>int(0),skip,goto(int(10)-int(5)),source([10-2:let(id(x),int(1)),10-3:let(id(y),int(2))],[10-4:let(id(x),int(2)),10-5:let(id(y),int(3))])):10-2,-,t,t),t(10-3,let(id(y),int(2)):10-4,-,t,t)),t(10-6,let(id(y),int(3)):void-void,-,t(10-5,let(id(x),int(2)):10-6,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,62,48,32,116,104,101,110,32,120,61,49,58,121,61,50,32,101,108,115,101,32,120,61,50,58,121,61,51],stack:[],status:end}).

test025 :-
  test(basic, run, "10 if 1<0 then x=1:y=2 else x=2:y=3", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),2,>,t,t(id(y),3,-,t,t)),program:t(10-4,goto(int(void)-int(void)):10-5,-,t(10-2,let(id(x),int(1)):10-3,-,t(10-1,if(int(1)<int(0),skip,goto(int(10)-int(5)),source([10-2:let(id(x),int(1)),10-3:let(id(y),int(2))],[10-4:let(id(x),int(2)),10-5:let(id(y),int(3))])):10-2,-,t,t),t(10-3,let(id(y),int(2)):10-4,-,t,t)),t(10-6,let(id(y),int(3)):void-void,-,t(10-5,let(id(x),int(2)):10-6,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,120,61,49,58,121,61,50,32,101,108,115,101,32,120,61,50,58,121,61,51],stack:[],status:end}).

test026 :-
  test(basic, run, "10 if 1<0 then x=1:y=2 else x=3:for i=1 to 3:x=x+1:next i", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),6,<,t(id(i),4,-,t,t),t),program:t(10-6,for(id(i),int(1),int(3),int(1),false):10-7,<,t(10-4,goto(int(void)-int(void)):10-5,<,t(10-2,let(id(x),int(1)):10-3,-,t(10-1,if(int(1)<int(0),skip,goto(int(10)-int(5)),source([10-2:let(id(x),int(1)),10-3:let(id(y),int(2))],[10-4:let(id(x),int(3)),10-5:for(id(i),int(1),int(3),int(1),false),10-6:let(id(x),id(x)+int(1)),10-7:next(id(i))])):10-2,-,t,t),t(10-3,let(id(y),int(2)):10-4,-,t,t)),t(10-5,let(id(x),int(3)):10-6,-,t,t)),t(10-8,next(id(i)):void-void,-,t(10-7,let(id(x),id(x)+int(1)):10-8,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'                                                  ',source:[49,48,32,105,102,32,49,60,48,32,116,104,101,110,32,120,61,49,58,121,61,50,32,101,108,115,101,32,120,61,51,58,102,111,114,32,105,61,49,32,116,111,32,51,58,120,61,120,43,49,58,110,101,120,116,32,105],stack:[],status:end}).

test027 :-
  test(basic, run, "10 def fnf(x)=x*log(x):print fnf(2)", comp{cursor:lc(1,0),data:void,line:void-void,mem:t(id(fnf),_494-_494*fn(log(_494)),-,t,t),program:t(10-2,print([fn(fnf(int(2)))]):void-void,-,t(10-1,defn(fnf,_494,x,_494*fn(log(_494))):10-2,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:' 1.38629                                          ',source:[49,48,32,100,101,102,32,102,110,102,40,120,41,61,120,42,108,111,103,40,120,41,58,112,114,105,110,116,32,102,110,102,40,50,41],stack:[],status:end}).

test028 :-
  test(basic, run, "10 x$=""asfd"":y$=x$:i=len(y$)", comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id('x$'),asfd,-,t(id(i),4,-,t,t),t(id('y$'),asfd,-,t,t)),program:t(10-3,let(id(i),fn(len(id('y$')))):void-void,<,t(10-2,let(id('y$'),id('x$')):10-3,<,t(10-1,let(id('x$'),str(asfd)):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                                  ',source:[49,48,32,120,36,61,34,97,115,102,100,34,58,121,36,61,120,36,58,105,61,108,101,110,40,121,36,41],stack:[],status:end}).

test029 :-
  test(basic, run, "10 a$ = ""0123456789""\n20 for i = len(a$)-1 to 0 step -1\n30 print mid$(a$,i,1);\n40 next i", comp{cursor:lc(1,0),data:void,line:void-void,mem:t(id('a$'),'0123456789',>,t,t(id(i),-1,-,t,t)),program:t(40-1,next(id(i)):void-void,<,t(20-1,for(id(i),fn(len(id('a$')))-int(1),int(0),-int(1),true):30-1,-,t(10-1,let(id('a$'),str('0123456789')):20-1,-,t,t),t(30-1,print([fn('mid$'(id('a$'),id(i),int(1))),;]):40-1,-,t,t)),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'9876543210                                        ',source:[49,48,32,97,36,32,61,32,34,48,49,50,51,52,53,54,55,56,57,34,10,50,48,32,102,111,114,32,105,32,61,32,108,101,110,40,97,36,41,45,49,32,116,111,32,48,32,115,116,101,112,32,45,49,10,51,48,32,112,114,105,110,116,32,109,105,100,36,40,97,36,44,105,44,49,41,59,10,52,48,32,110,101,120,116,32,105],stack:[],status:end}).

test030 :-
  screen:set_screen_panel_size(2,4),
  test(basic, run, "10 print ""d123"";", comp{cursor:lc(1,0),data:void,line:void-void,mem:t,program:t(10-1,print([str(d123),;]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'d123    ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,51,34,59],stack:[],status:end}).

test031 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d123""",comp{cursor:lc(1,0),data:void,line:void-void,mem:t,program:t(10-1,print([str(d123)]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'        ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,51,34],stack:[],status:end}).

test032 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d12""",comp{cursor:lc(1,0),data:void,line:void-void,mem:t,program:t(10-1,print([str(d12)]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'d12     ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,34],stack:[],status:end}).

test033 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d12"";",comp{cursor:lc(0,3),data:void,line:void-void,mem:t,program:t(10-1,print([str(d12),;]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'d12     ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,34,59],stack:[],status:end}).

test034 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d123456"";",comp{cursor:lc(1,3),data:void,line:void-void,mem:t,program:t(10-1,print([str(d123456),;]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'d123456 ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,51,52,53,54,34,59],stack:[],status:end}).

test035 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d1234567890"";",comp{cursor:lc(1,3),data:void,line:void-void,mem:t,program:t(10-1,print([str(d1234567890),;]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'4567890 ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,51,52,53,54,55,56,57,48,34,59],stack:[],status:end}).

test036 :-
  screen:set_screen_panel_size(2,4),
  test(basic,run,"10 print ""d123456789012345678"";",comp{cursor:lc(1,3),data:void,line:void-void,mem:t,program:t(10-1,print([str(d123456789012345678),;]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'2345678 ',source:[49,48,32,112,114,105,110,116,32,34,100,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,34,59],stack:[],status:end}).

test037 :-
  screen:set_screen_panel_size(10,10),
  test(basic,run,"10 for i=1 to 9: for j=1 to i: print ""*"";:next j: print: next i",comp{cursor:lc(9,0),data:void,line:void-void,mem:t(id(i),10,>,t,t(id(j),10,-,t,t)),program:t(10-4,next(id(j)):10-5,-,t(10-2,for(id(j),int(1),id(i),int(1),false):10-3,-,t(10-1,for(id(i),int(1),int(9),int(1),false):10-2,-,t,t),t(10-3,print([str(*),;]):10-4,-,t,t)),t(10-6,next(id(i)):void-void,-,t(10-5,print([]):10-6,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'*         **        ***       ****      *****     ******    *******   ********  *********           ',source:[49,48,32,102,111,114,32,105,61,49,32,116,111,32,57,58,32,102,111,114,32,106,61,49,32,116,111,32,105,58,32,112,114,105,110,116,32,34,42,34,59,58,110,101,120,116,32,106,58,32,112,114,105,110,116,58,32,110,101,120,116,32,105],stack:[],status:end}).

% test038 :- % Better,do not support this (string concatenation without operator)
%   screen:set_screen_panel_size(4,10),
%   test(basic,run,"10 x=-1\n20 print ""x=""x""km""",comp{cursor:lc(1,0),data:void,line:void-void,mem:t(id(x),-1,-,t,t),program:t(20-1,print([str('x='),;,id(x),str(km)]):void-void,-,t(10-1,let(id(x),-int(1)):20-1,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'x=-1km                                  ',stack:[],status:end}).

test038 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 dim a(2):a(0)=0:x=a(0)",comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(a(0)),0,<,t(id(x),0,-,t,t),t),program:t(10-3,let(id(x),id(a(int(0)))):void-void,<,t(10-2,let(id(a(int(0))),int(0)):10-3,<,t(10-1,dim(id(a),[int(2)]):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,100,105,109,32,97,40,50,41,58,97,40,48,41,61,48,58,120,61,97,40,48,41],stack:[],status:end}).

% Array copy
test039 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 dim a$(2):a$(0)=""0"":b$=a$",comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id('b$'),'',-,t(id('a$'),'',-,t,t),t(id('a$'(0)),'0',-,t,t)),program:t(10-3,let(id('b$'),id('a$')):void-void,<,t(10-2,let(id('a$'(int(0))),str('0')):10-3,<,t(10-1,dim(id('a$'),[int(2)]):10-2,-,t,t),t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,100,105,109,32,97,36,40,50,41,58,97,36,40,48,41,61,34,48,34,58,98,36,61,97,36],stack:[],status:end}).

% Access to a non-initialized numeric array element. Created as 0
test040 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 dim a(2): x=a(1): locate 1,3: print x",comp{cursor:lc(2,0),data:void,line:void-void,mem:t(id(a(1)),0,<,t(id(x),0,-,t,t),t),program:t(10-4,print([id(x)]):void-void,<,t(10-2,let(id(x),id(a(int(1)))):10-3,-,t(10-1,dim(id(a),[int(2)]):10-2,-,t,t),t(10-3,locate(int(1),int(3)):10-4,-,t,t)),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'              0                         ',source:[49,48,32,100,105,109,32,97,40,50,41,58,32,120,61,97,40,49,41,58,32,108,111,99,97,116,101,32,49,44,51,58,32,112,114,105,110,116,32,120],stack:[],status:end}).

% Access to a non initialized string array element. Created as the empty string ""
test041 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 dim a$(2): x$=a$(1)",comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id('a$'(1)),'',<,t(id('x$'),'',-,t,t),t),program:t(10-2,let(id('x$'),id('a$'(int(1)))):void-void,-,t(10-1,dim(id('a$'),[int(2)]):10-2,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,100,105,109,32,97,36,40,50,41,58,32,120,36,61,97,36,40,49,41],stack:[],status:end}).

test042 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 gosub 20:end\n20 print 1:return",comp{cursor:lc(1,0),data:void,line:20-1,mem:t,program:t(20-2,return:void-void,<,t(10-2,end:20-1,-,t(10-1,gosub(int(20)-int(1)):10-2,-,t,t),t(20-1,print([int(1)]):20-2,-,t,t)),t(void-void,end:void-void,-,t,t)),runline:10-2,screen:' 1                                      ',source:[49,48,32,103,111,115,117,98,32,50,48,58,101,110,100,10,50,48,32,112,114,105,110,116,32,49,58,114,101,116,117,114,110],stack:[],status:end}).

test043 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 a=1:run 20",comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-2,run(int(20),true):void-void,-,t(10-1,let(id(a),int(1)):10-2,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,97,61,49,58,114,117,110,32,50,48],stack:[],status:end}).

test044 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 a=1:new",comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(void-void,end:void-void,-,t,t),runline:void-void,screen:'                                        ',source:[],stack:[],status:end}).

test045 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 a=1:clear",comp{cursor:lc(0,0),data:void,line:void-void,mem:t,program:t(10-2,clear:void-void,-,t(10-1,let(id(a),int(1)):10-2,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,97,61,49,58,99,108,101,97,114],stack:[],status:end}).

test046 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 a=1:on a goto 30,40\n20 print 0:end\n30 print 1:end\n40 print 2:end",comp{cursor:lc(1,0),data:void,line:40-1,mem:t(id(a),1,-,t,t),program:t(30-2,end:40-1,<,t(20-2,end:30-1,<,t(10-2,goto(id(a),[int(30),int(40)]):20-1,-,t(10-1,let(id(a),int(1)):10-2,-,t,t),t(20-1,print([int(0)]):20-2,-,t,t)),t(30-1,print([int(1)]):30-2,-,t,t)),t(40-2,end:void-void,-,t(40-1,print([int(2)]):40-2,-,t,t),t(void-void,end:void-void,-,t,t))),runline:30-2,screen:' 1                                      ',source:[49,48,32,97,61,49,58,111,110,32,97,32,103,111,116,111,32,51,48,44,52,48,10,50,48,32,112,114,105,110,116,32,48,58,101,110,100,10,51,48,32,112,114,105,110,116,32,49,58,101,110,100,10,52,48,32,112,114,105,110,116,32,50,58,101,110,100],stack:[],status:end}).

test047 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 restore\n20 data 1,2\n30 data 2,3\n40 read a,b,c,d: restore 30: read e,f",comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(d),3,-,t(id(b),2,-,t(id(a),1,-,t,t),t(id(c),2,-,t,t)),t(id(e),2,>,t,t(id(f),3,-,t,t))),program:t(40-1,read([id(a),id(b),id(c),id(d)]):40-2,-,t(20-1,data(t(2,int(2),<,t(1,int(1),-,t,t),t),30-1):30-1,-,t(10-1,restore(int(1),false):20-1,-,t,t),t(30-1,data(t(2,int(3),<,t(1,int(2),-,t,t),t),void):40-1,-,t,t)),t(40-3,read([id(e),id(f)]):void-void,-,t(40-2,restore(int(30),true):40-3,-,t,t),t(void-void,end:void-void,-,t,t))),runline:void-void,screen:'                                        ',source:[49,48,32,114,101,115,116,111,114,101,10,50,48,32,100,97,116,97,32,49,44,50,10,51,48,32,100,97,116,97,32,50,44,51,10,52,48,32,114,101,97,100,32,97,44,98,44,99,44,100,58,32,114,101,115,116,111,114,101,32,51,48,58,32,114,101,97,100,32,101,44,102],stack:[],status:end}).

test048 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 data 1\n20 read a",comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(a),1,-,t,t),program:t(20-1,read([id(a)]):void-void,-,t(10-1,data(t(1,int(1),-,t,t),void):20-1,-,t,t),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,100,97,116,97,32,49,10,50,48,32,114,101,97,100,32,97],stack:[],status:end}).

test049 :-
  screen:set_screen_panel_size(4,10),
  test(basic,run,"10 data 1\n20 read a,b",failure(error('Runtime','No more data to read',_))).

test050 :-
  screen:set_screen_panel_size(5,10),
  see('test/test050_input'),
  test(basic,run,"10 input ""a,a$="";a,a$",comp{cursor:lc(2,0),data:void,line:void-void,mem:t(id(a),110.00000000000001,>,t,t(id('a$'),'1.1e2',-,t,t)),program:t(10-1,input(str('a,a$='),[id(a),id('a$')]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'a,a$= 110 1.1e2                                   ',source:[49,48,32,105,110,112,117,116,32,34,97,44,97,36,61,34,59,97,44,97,36],stack:[],status:end}),
  seen.

test051 :-
  screen:set_screen_panel_size(4,10),
  see('test/test051_input'),
  test(basic,run,"10 input a",failure(error('Runtime','Invalid input',_))),
  seen.

test052 :-
  screen:set_screen_panel_size(4,10),
  see('test/test052_input'),
  test(basic,run,"10 input a",comp{cursor:lc(1,0),data:void,line:void-void,mem:t(id(a),123,-,t,t),program:t(10-1,input(str(''),[id(a)]):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:' 123                                    ',source:[49,48,32,105,110,112,117,116,32,97],stack:[],status:end}),
  seen.

test_renum(Program,Comp) :-
  lex_parse(Program, StatementList),
  init_comp(Program, StatementList, Comp0),
  tools:renum(1, 1, Comp0-Comp1),
  interpreter:interpret_statement(list(int(1), false), Comp1-Comp).

test053 :-
  screen:set_screen_panel_size(6,15),
  test(basic,test_renum,"10 print 1\n20 goto 30\n30 print 2\n40 stop\n50 end",comp{cursor:lc(5,0),data:void,line:1-1,mem:t,program:t(4-1,stop:5-1,-,t(2-1,goto(int(3)-int(1)):3-1,-,t(1-1,print([int(1)]):2-1,-,t,t),t(3-1,print([int(2)]):4-1,-,t,t)),t(void-void,end:void-void,<,t(5-1,end:void-void,-,t,t),t)),runline:void-void,screen:'1 PRINT 1      2 GOTO 3       3 PRINT 2      4 STOP         5 END                         ',source:[49,48,32,112,114,105,110,116,32,49,10,50,48,32,103,111,116,111,32,51,48,10,51,48,32,112,114,105,110,116,32,50,10,52,48,32,115,116,111,112,10,53,48,32,101,110,100],stack:[],status:run}),
  seen.


