/*********************************************************/
/*                                                       */
/* COMP_OBJ module for the BASIC language                */
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

:- module(comp_obj,
          [ empty_mem/1,
            clear_mem/1,
            empty_array/1,
            empty_program/1,
            label_to_data_address/2,
            find_data/3,
            set_last_void_statement/2,
            init_comp/2,
            init_comp/3,
            get/3,
            get_last/3,
            get_source/2,
            get_screen/2,
            get_cursor/2,
            get_data/2,
            get_mem/3,
            get_line/2,
            get_runline/2,
            get_next_line/3,
            get_program_start_line/2,
            get_program/2,
            get_program_statements/2,
            get_status/2,
            get_statement_and_update_lines/2,
            get_statement_and_next_line/4,
            get_statement/3,
            set/4,
            set_line/2,
            set_runline/2,
            set_mem/2,
            set_mem/3,
            set_program/2,
            set_screen/2,
            set_cursor/2,
            set_stack/2,
            set_data/2,
            set_status/2,
            empty_stack/1,
            push/2,
            pop/2
%             empty_queue/1,
%             enqueue/2,
%             dequeue/2
          ]).


:- use_module(misc, [ ceiling_line/3 ]).

:- use_module(basic,
          [ lex_parse_transform/2 ]).

:- use_module(error_,
          [ reset_error/0,
            set_error/3 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fundamental operations of the comp object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gen_comp(+Source, +Screen, +Cursor, +Mem, +Stack, +Program, +Line, +RunLine, +Data, +Status, -Comp)
% Generate a comp object in terms of its components:
%   Source  : Program source (either a file or a list of codes)
%   Screen : An atom with size Lines*Columns
%   Cursor : A term lc(Line, Column), with base 0 for lines and columns
%   Mem    : An association list with Key-Value, where Key is a variable identifier, and Value is its value. If the variable is an array, its value is the term array(Dimensions, Values), where Dimensions is the list of its dimensions, and Values is an association list with Key-Value, where Key is the array index (a list, base-0 elements), and Value is its value
%   Stack  : A list with basic operations push and pop.
%   Program: An association list with Key-Value, where Key is a program address Line-Statement (each line can contain several statements separated by ":"). Contain variables (number, string and array)
%   Line   : Program statement to be executed of the form Line-Statement (numbers with base-1)
%   RunLine: Program statement which is currently running (with the same format as Line). 
%   Data   : Address of the current data to read of the form Line-Statement-Element (numbers with base-1)
%   Status : Computation status, either run (running) or end (stopped) 
gen_comp(Source, Screen, Cursor, Mem, Stack, Program, Line, RunLine, Data, Status, comp{source: Source, screen:Screen, cursor:Cursor, mem:Mem, stack:Stack, program:Program, line:Line, runline:RunLine, data:Data, status:Status}).

empty_mem(S) :-
  empty_assoc(S).

clear_mem(Comp0-Comp) :-
  empty_mem(M),
  set_mem(M, Comp0-Comp).

empty_array(Array) :-
  empty_assoc(Array).

empty_program(Program) :-
  empty_assoc(Program0),
  set_last_void_statement(Program0, Program).

% label_to_data_address(+Label, -Data)
% Return the data address corresponding to program label
% A data address is of the form Line-Statement-Element (base 1)
label_to_data_address(void, void) :-
  !.
label_to_data_address(LineStmt, LineStmt-1).

% find_data(+Comp, +Line, -Data)
% Find the data address for the first sentence starting at Line
find_data(Comp, Line, Data) :-
  ceiling_line(Comp, Line-1, Label),
  find_data_aux(Comp, Label, Data).

find_data_aux(_Comp, void-void, void) :-
  !.
find_data_aux(Comp, Label, Data) :-
  get_statement_and_next_line(Comp, Label, Statement, NextLabel),
  (Statement = data(_, _)
   -> Data = Label-1
   ;  find_data_aux(Comp, NextLabel, Data)).

set_last_void_statement(Tree0, Tree) :-
  set(Tree0, void-void, end:void-void, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(S, Var, Val) :-
  get_assoc(Var, S, Val).

get_last(S, Var, Val) :-
  max_assoc(S, Var, Val).

get_source(Comp, Comp.source).

get_screen(Comp, Comp.screen).

get_cursor(Comp, Comp.cursor).

get_data(Comp, Comp.data).

get_mem(Comp, Var, Val) :-
  get(Comp.mem, Var, Val).

get_line(Comp, Comp.line).

get_runline(Comp, Comp.runline).

get_next_line(Comp, Line, NextLine) :-
  get(Comp.program, Line, _Command:NextLine).

% Find the starting line (the node in the tree with the lowest key).
get_program_start_line(Program, Start) :-
  min_assoc(Program, Start, _).

get_program(Comp, Comp.program).

% get_program_statements(+Comp, -StatementList)
% Get the list of program statements in Comp, 
%   excepting the last (void) one
get_program_statements(Comp, StatementList) :-
  assoc_to_list(Comp.program, AllStatementList),
  findall(Line:Stmt, 
     (member(Line-(Stmt:_Next), AllStatementList), 
      Line \== void-void), 
    StatementList).

get_status(Comp, Comp.status).

% get_statement_and_update_lines(-Command, +Comp0--Comp)
% Get the next statement to be executed, which is pointed by Comp0.Line,
% and update both the running line and the next line labels
get_statement_and_update_lines(Command, Comp-Comp.put([runline:Comp.line, line:NewLine])) :-
  get(Comp.program, Comp.line, Command:NewLine).

% get_statement_and_next_line(+Comp, +Label, -Statement, -NextLine)
% Get the statement at address Label and return the next line label
get_statement_and_next_line(Comp, Label, Statement, NextLine) :-
  get(Comp.program, Label, Statement:NextLine).

% get_statement(+Comp, +Label, -Command, -NextLine)
% Get the statement at line Label
get_statement(Comp, Label, Statement) :-
  get(Comp.program, Label, Statement:_NextLine).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set(S0, Var, Val, S) :-
  put_assoc(Var, S0, Val, S),
  !.

% This is not exported (but called from) since there is a same predicate in screen module
set_cursor(Cursor, Comp-Comp.put([cursor:Cursor])).

set_line(Line, Comp-Comp.put([line:Line])).

set_runline(Line, Comp-Comp.put([runline:Line])).

set_mem(Var, Val, Comp0-Comp) :-
  set(Comp0.mem, Var, Val, NewMemory),
  set_mem(NewMemory, Comp0-Comp).

set_mem(Memory, Comp-Comp.put([mem:Memory])).

set_program(Program, Comp-Comp.put([program:Program])).

set_screen(Screen, Comp-Comp.put([screen:Screen])).

set_stack(Stack, Comp-Comp.put([stack:Stack])).

set_data(Data, Comp-Comp.put([data:Data])).

set_status(Status, Comp-Comp.put([status:Status])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STACK predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_stack([]).

push(E, Comp-Comp.put([stack:[E|Comp.stack]])).
	
pop(E, Comp-Comp.put([stack:Stack])) :-
  [E|Stack] = Comp.stack.

% % QUEUE predicates

% empty_queue(q([], [])).

% enqueue(X, q(Ls, Rs)-q(Ls, [X|Rs])).

% dequeue(X, q([], Ls )-q(Xs, [])) :-
%   reverse(Ls, [X|Xs]).
% dequeue(X, q([X|Ls], Rs)-q(Ls, Rs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INIT Comp object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_comp(+Source, -Comp)
% Initialize the comp object from Source (either a file or a list of codes).
init_comp(Source, Comp) :- 
  lex_parse_transform(Source, StatementList),
  sort(StatementList, OrderedStatementList),
  init_comp(Source, OrderedStatementList, Comp).

% init_comp(+Source, +StatementList, -Comp)
% Initialize the comp object with the given statement list,
% which has its source in Source.
init_comp(Source, [], Comp) :-
  !,
  empty_program(Program),
  init_comp(Source, Program, void, Comp).
init_comp(Source, StatementList, Comp) :-
  statement_list_to_program(StatementList, FirstDataLabel, Program),
  label_to_data_address(FirstDataLabel, StartData),
  init_comp(Source, Program, StartData, Comp).

% init_comp(+Source, +Program, +FirstData, -Comp)
% Initialize the comp object with the given source, program and data address.
init_comp(Source, Program, FirstData, Comp) :-
  screen:init_screen(Sc, C),
  empty_mem(M),
  empty_stack(St),
  get_program_start_line(Program, Line),
  gen_comp(Source, Sc, C, M, St, Program, Line, void-void, FirstData, run, Comp).

% statement_list_to_program(+ProgramList, -DataLabel, -ProgramTree)
% Parse a list of statements to the internal tree format, linking 
% data statements. Each data statement points to the next one,
% and the last one points at void-void. Return the first data line
statement_list_to_program([N:S], LastData, Tree) :-
  link_data(N:S, LastData, void),
  empty_assoc(Tree0),
  set(Tree0, N, S:void-void, Tree1),
  set_last_void_statement(Tree1, Tree).
statement_list_to_program([N-I1:_S1, N-I2:_S2|_Rest], _LastData, _Tree) :-
  I2 =< I1,
  !,
  format(atom(Error), 'Duplicated line number ~w', [N]),
  reset_error,
  set_error('Semantic', Error, pos(void, void)),
  fail.
statement_list_to_program([N1:S1, N2:S2|Rest], LastData, Tree) :-
  link_data(N1:S1, LastData, NextData),
  statement_list_to_program([N2:S2|Rest], NextData, Tree0),
  set(Tree0, N1, S1:N2, Tree).

% link_data(+Statement, -LastDataLabel, -NextDataLabel)
% Link data statements. 
link_data(N:data(_, NextData), N, NextData) :-
  !.
link_data(_NS, Data, Data).


