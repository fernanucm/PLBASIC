/*********************************************************/
/*                                                       */
/* INTERPRETER for the BASIC language                    */
/*    Version Seiko Data 2000                            */
/*                                                       */
/* Extension of:                                         */
/*    %Version 0.1 of PROLOG BASIC.                      */
/*    %Author: Victor Lagerkvist.                        */
/*    %License: not sure, as long as I'm not responsible */
/*    %for anything.                                     */
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

:- module(interpreter,
          [ interpret_statement/2 ]).

:- use_module(library(assoc)).

:- use_module(comp_obj).

:- use_module(list,
          [ interpret_list/2 ]).

:- use_module(misc).

:- use_module(test,
          [ test/4 ]).

:- use_module(screen,
          [ screen_panel_location/1,
            print_to_screen/2,
            init_screen/2,
            valid_cursor/1 ]).

:- use_module(lexer,
          [ lex/2 ]).

:- use_module(parser,
          [ expr/3,
            is_number/1 ]).

:- use_module(error_,
          [ set_error/3,
            process_error/0 ]).

% Foreign library implementing Windows kbhit/1 for INKEY$
:- (current_prolog_flag(windows, true) -> load_foreign_library('extern/extern.dll') ; true).

% This SWI-Prolog flag makes strings delimited by double
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).

% Boolean operators which are different in Prolog and BASIC
:- op(700, xfx, <>).
:- op(700, xfx, <=).


% interpret_statement(+Statement, +Comp0--Comp)
% Interpret a single statement.
interpret_statement(run(LineExp, _), Comp0-Comp) :-
  !,
  clear_mem(Comp0-Comp1),
  line_to_label_exp(LineExp, LabelExp),
  interpret_statement(goto(LabelExp), Comp1-Comp).
interpret_statement(list(LineExp, _), Comp0-Comp) :-
  !,
  eval_exp(LineExp, Line, Comp0-Comp1),
  line_to_label(Line, Label),
  ceiling_line(Comp1, Label, ListLabel),
  interpret_list(ListLabel, Comp1-Comp).
interpret_statement(new, _Comp0-Comp) :-
  !,
  init_comp("", Comp0),
  set_status(new, Comp0-Comp).
interpret_statement(clear, Comp0-Comp) :-
  !,
  clear_mem(Comp0-Comp).
interpret_statement(cont, Comp0-Comp) :-
  !,
  set_status(run, Comp0-Comp).
interpret_statement(dim(_Id, Dimensions), Comp0-Comp) :-
  !,
  eval_exps(Dimensions, Vs, Comp0-Comp),
  check_array_dimensions(Vs, Comp).
interpret_statement(let(Id, E), Comp0-Comp) :-
  !,
  interpret_let(Id, E, Comp0-Comp).
interpret_statement(defn(Fn, Param, _Name, Expr), Comp0-Comp) :-
  !,
  set_mem(id(Fn), Param-Expr, Comp0-Comp).
interpret_statement(cls, Comp0-Comp) :-
  !,
  init_screen(Screen, Cursor),
  set_screen(Screen, Comp0-Comp1),
  comp_obj:set_cursor(Cursor, Comp1-Comp).
interpret_statement(beep, Comp-Comp) :-
  !,
  put_code(7).
interpret_statement(rem(_Remark), Comp-Comp) :-
  !.
interpret_statement(for(Id, Start, End, Step, _OptStep), Comp0-Comp) :-
  !,
  eval_exp(Start, StartVal, Comp0-Comp1),
  set_mem(Id, StartVal, Comp1-Comp2),
	interpret_for(1, for(Id, Start, End, Step), void, Comp2-Comp).
% NEXT: Pop a for-statement from the stack, increase the for variable,
% evaluate the Boolean expression, set the current line to the line of
% the for statement, and interpret the for statement (while informing the
% for statement that the line of the next statement is CurrentLine).
interpret_statement(next(Id), Comp0-Comp) :-
  !,
	pop(ForLine:for(Id, Start, End, Step), Comp0-Comp1),
	eval_exp(Id + Step, NewVal, Comp1-Comp2),
	set_mem(Id, NewVal, Comp2-Comp3),
	eval_exp(Id * Step <= End * Step, Res, Comp3-Comp4),
	get_line(Comp2, LineAfterNext),
	set_line(ForLine, Comp4-Comp5),
	interpret_for(Res, for(Id, Start, End, Step), LineAfterNext, Comp5-Comp).
interpret_statement(goto(LabelExp), Comp0-Comp) :-
  !,
  interpret_goto(LabelExp, Comp0-Comp).
interpret_statement(goto(IntExp, GotoExps), Comp0-Comp) :-
  !,
  eval_exp(IntExp, Int, Comp0-Comp1),
  eval_exps(GotoExps, Gotos, Comp1-Comp2),
  (nth1(Int, Gotos, Goto)
   -> label_exp(Goto-1, GotoExp),
      interpret_goto(GotoExp, Comp2-Comp)
   ;  Comp = Comp2).
interpret_statement(gosub(LabelExp), Comp0-Comp) :-
  !,
	get_line(Comp0, Line),
	push(Line, Comp0-Comp1),
	interpret_statement(goto(LabelExp), Comp1-Comp).
interpret_statement(gosub(IntExp, GosubExps), Comp0-Comp) :-
  !,
  eval_exp(IntExp, Int, Comp0-Comp1),
  eval_exps(GosubExps, Gosubs, Comp1-Comp2),
  (nth1(Int, Gosubs, Gosub)
   -> label_exp(Gosub-1, GosubExp),
      interpret_statement(gosub(GosubExp), Comp2-Comp) % REVIEW
   ;  Comp = Comp2).
interpret_statement(return, Comp0-Comp) :-
  !,
	pop(Line, Comp0-Comp1),
	set_line(Line, Comp1-Comp).
interpret_statement(if(B, Then, Else, _List), Comp0-Comp) :-
  !,
  eval_bool(B, Res, Comp0-Comp1),
	interpret_if(Res, Then, Else, Comp1-Comp).
interpret_statement(print(Exprs), Comp0-Comp) :-
  !,
  interpret_print(Exprs, Comp0-Comp).
interpret_statement(locate(LE, CE), Comp0-Comp) :-
  !,
	eval_exp(fn(int(LE)), L, Comp0-Comp1),
	eval_exp(fn(int(CE)), C, Comp1-Comp2),
	(valid_cursor(lc(L, C))
	 -> comp_obj:set_cursor(lc(L, C), Comp2-Comp)
	 ;  Comp = Comp2,
	    atomic_list_concat(['Non valid cursor position: ', L, ', ', C], Msg),
	    set_error('Runtime', Msg, Comp2),
	    !, fail).
interpret_statement(input(MsgExpr, Variables), Comp0-Comp) :-
  !,
  eval_exp(MsgExpr, Msg, Comp0-Comp1),
  print_to_screen(val(Msg), Comp1-Comp2),
  interpret_input_variables(Variables, Comp2-Comp).
interpret_statement(data(_, _), Comp-Comp) :-
  !.
interpret_statement(read(Variables), Comp0-Comp) :-
  !,
  interpret_read(Variables, Comp0-Comp).
interpret_statement(restore(LineExp, _), Comp0-Comp) :-
  !,
	eval_exp(LineExp, Line, Comp0-Comp1),
	find_data(Comp1, Line, Data),
  set_data(Data, Comp1-Comp).
interpret_statement(skip, Comp-Comp) :-
  !.
interpret_statement(stop, Comp0-Comp) :-
  !,
  set_status(stop, Comp0-Comp).
interpret_statement(end, Comp0-Comp) :-
  !,
  set_status(end, Comp0-Comp).
% Unsupported statements in Seiko Data 2000 from here on
interpret_statement(pause(SecondsExp), Comp0-Comp) :-
  !,
  eval_exp(SecondsExp, Seconds, Comp0-Comp),
  sleep(Seconds).
interpret_statement(Statement, Comp-Comp) :-
  format('Interpreter Error: Unsupported statement ~w\n', [Statement]),
  !,
  fail.

% check_array_dimensions(+Vs)
check_array_dimensions([], _Comp) :-
  !.
check_array_dimensions([D|Ds], Comp) :-
  between(0, 65535, D),
  !,
  check_array_dimensions(Ds, Comp).
check_array_dimensions(_Ds, Comp) :-
	set_error('Runtime', 'Invalid array index', Comp),
  !, fail.


% interpret_let(+Variable, -Expression, +Comp0--Comp)
interpret_let(Id, E, Comp0-Comp) :-
  id_array_name_index(Id, Name, Index),
  !,
  eval_exps(Index, EIndex, Comp0-Comp1),
  eval_exp(E, V, Comp1-Comp2),
  EId =.. [Name|EIndex],
  set_mem(id(EId), V, Comp2-Comp).
interpret_let(Id, E, Comp0-Comp) :-
  eval_exp(E, V, Comp0-Comp1),
  set_mem(Id, V, Comp1-Comp).


% interpret_print(+Expressions, +Comp--Comp)
% Print the given list of expressions, which include:
%   ';' : Do not move the cursor location
%   'nl': New line
%   ',' : Start at the next line
%   Expr: Print the result of evaluating Expr
interpret_print([';'], Comp-Comp) :-
  !.
interpret_print([], Comp0-Comp) :-
  !,
  print_to_screen(nl, Comp0-Comp).
interpret_print([';'|Exprs], Comp0-Comp) :-
  !,
  interpret_print(Exprs, Comp0-Comp).
interpret_print([','|Exprs], Comp0-Comp) :-
  !,
  print_to_screen(tab, Comp0-Comp1),
  interpret_print(Exprs, Comp1-Comp).
interpret_print([Expr|Exprs], Comp0-Comp) :-
  eval_exp(Expr, V, Comp0-Comp1),
  print_to_screen(val(V), Comp1-Comp2),
  interpret_print(Exprs, Comp2-Comp).


% interpret_input_variables(+Variables)
interpret_input_variables([], Comp-Comp).
interpret_input_variables([id(Variable)|Variables], Comp0-Comp) :-
  % current_input(Stream),
  get_cursor(Comp0, lc(CL, CC)),
  screen_panel_location(lc(SL, SC)),
  L is CL + SL + 1, % + 1 because of the 1-character-width frame
  C is CC + SC + 1,
  write_at(L, C),
	my_read_line_to_codes(Codes),
	(is_str_type_var(Variable)
	 -> atom_codes(Value, Codes),
	    E = str(Value),
	    Comp1 = Comp0
	 ;  (lex(Codes, Tokens),
	     phrase(expr(E), Tokens),
	     eval_exp(E, Value, Comp0-Comp1),
	     is_number(E)
	     -> true
	     ;  set_error('Runtime', 'Invalid input', Comp0),
	    !, fail)),
  set_mem(id(Variable), Value, Comp1-Comp2),
  interpret_print([E], Comp2-Comp3),
	interpret_input_variables(Variables, Comp3-Comp).

% interpret_read(+Variables, +Comp0--Comp)
interpret_read([], Comp-Comp) :-
  !.
interpret_read([Variable|Variables], Comp0-Comp) :-
  get_data(Comp0, DataAddress),
  (DataAddress == void
   -> set_error('Runtime', 'No more data to read', Comp0),
      !, fail
   ;  get_data_and_increment_address(DataAddress, Expression, Comp0-Comp1),
      interpret_let(Variable, Expression, Comp1-Comp2),
      interpret_read(Variables, Comp2-Comp)).

% get_data_and_increment_address(+Address, -Expression, +Comp0--Comp)
% Get the current data and increment the data address
get_data_and_increment_address(Line-Stmt-Index, Expression, Comp0-Comp) :-
  get_statement(Comp0, Line-Stmt, data(Elements, NextLabel)),
  get(Elements, Index, Expression),
  (get_last(Elements, Index, Expression)
   -> label_to_data_address(NextLabel, Data),
      set_data(Data, Comp0-Comp)
   ;  Index1 is Index+1,
      set_data(Line-Stmt-Index1, Comp0-Comp)).


% interpret_goto(+LabelExp, +Comp0--Comp)
interpret_goto(LabelExp, Comp0-Comp) :- % REVIEW. Errors such as goto 1.1
  eval_label_exp(LabelExp, Label, Comp0-Comp1),
  % The line might not exist, so find the least upper bound
  ceiling_line(Comp0, Label, Goto),
	set_line(Goto, Comp1-Comp).


% interpret_for(+Bool, +For, +LineAfterNext, +Comp0--Comp)
interpret_for(0, for(_,_,_,_), LineAfterNext, Comp0-Comp) :-
	label_exp(LineAfterNext, ExpLineAfterNext),
	interpret_statement(goto(ExpLineAfterNext), Comp0-Comp).
interpret_for(1, for(Id, Start, End, Step), _LineAfterNext, Comp0-Comp) :-
	get_line(Comp0, ForLine),
	push(ForLine:for(Id, Start, End, Step), Comp0-Comp).

% interpret_if(+Bool, +Then, +Else, +Comp0--Comp)
interpret_if(0, _Then, Else, Comp0-Comp) :-
  interpret_statement(Else, Comp0-Comp).
interpret_if(1, Then, _Else, Comp0-Comp) :-
  interpret_statement(Then, Comp0-Comp).

%% Predicates evaluating arithmetic, string and Boolean expressions.

% eval_exp(+Expr, -Value, +Comp--Comp)
eval_exp(int(Int), Exp, Comp-Comp) :-
  !,
  eval_int(Int, Exp).
eval_exp(frac(Int, Frac), Exp, Comp-Comp) :-
  !,
  eval_frac(Int, Frac, Exp).
eval_exp(float(Int, Frac, Exp), Float, Comp-Comp) :-
  !,
  eval_float(Int, Frac, Exp, Float).
eval_exp(str(Str), Str, Comp-Comp) :-
  !.
eval_exp(id(Array), V, Comp0-Comp) :- % Array element
  id_array_name_index(id(Array), Name, Index),
  !,
  eval_exps(Index, Is, Comp0-Comp1),
  EArray =.. [Name|Is],
  eval_var(id(EArray), V, Comp1-Comp).
eval_exp(id(Id), V, Comp0-Comp) :- % Non-array Variable
  eval_var(id(Id), V, Comp0-Comp).
%   (get_mem(Comp, id(Name), V)
%    -> true
%    ;  set_error('Runtime', 'Undefined variable', Comp),
%       !, fail).
% eval_exp(id(Name, Index), V, Comp0-Comp) :- % Array element
%   eval_exps(Index, Is, Comp0-Comp1),
%   (get_mem(Comp1, id(Name), array(Dim, Array0))
%    -> (get(Array0, Is, V)
%        -> Comp = Comp1
%        ;  var_default_value(Name, V), % First-time access to an array element
%           set(Array0, Is, V, Array),
%           set_mem(id(Name), array(Dim, Array), Comp1-Comp))
%    ;  set_error('Runtime', 'Undefined variable', Comp1),
%       !, fail).
eval_exp(E1 + E2, V, Comp0-Comp) :-
  !,
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_plus(V1, V2, V).
eval_exp(E1 - E2, V, Comp0-Comp) :-
  !,
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_sub(V1, V2, V).
eval_exp(E1 * E2, V, Comp0-Comp) :-
  !,
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_mult(V1, V2, V).
eval_exp(E1 / E2, V, Comp0-Comp) :-
  !,
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_div(V1, V2, V, Comp).
eval_exp(E1 ^ E2, V, Comp0-Comp) :-
  !,
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_pow(V1, V2, V).
eval_exp(- E, V, Comp0-Comp) :-
  !,
  eval_exp(E, V1, Comp0-Comp),
  eval_sgn(V1, V).
eval_exp(fn('fre'(_)), 2922, Comp-Comp) :-
  !.
eval_exp(fn('rnd'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, S, Comp0-Comp),
  (S < 0
   -> PS is -S,
      set_random(seed(PS))
   ;  (S == 0
       -> random_state(State)
       ;  true)),
   set_random_state(State),
   V is random_float.
eval_exp(fn('inkey$'), V, Comp-Comp) :-
  !,
  kbhit(Code),
  (Code == 0
   -> V = ''
   ;  char_code(V, Code)).
eval_exp(fn('asc'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, V1, Comp0-Comp),
  atom_codes(V1, [V|_]).
eval_exp(fn('chr$'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, V1, Comp0-Comp),
  atom_codes(V, [V1]).
eval_exp(fn('val'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, V1, Comp0-Comp1),
  atom_codes(V1, Cs),
  lex(Cs, Tks),
  expr(ST, Tks, []),
  eval_exp(ST, V, Comp1-Comp).
eval_exp(fn('str$'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, V1, Comp0-Comp),
  number_codes(V1, Cs),
  atom_codes(V, Cs).
eval_exp(fn('left$'(E1, E2)), V, Comp0-Comp) :-
  !,
  eval_exp(E1, Str, Comp0-Comp1),
  eval_exp(E2, N, Comp1-Comp),
  sub_atom(Str, 0, N, _, V).
eval_exp(fn('right$'(E1, E2)), V, Comp0-Comp) :-
  !,
  eval_exp(E1, Str, Comp0-Comp1),
  eval_exp(E2, N, Comp1-Comp),
  sub_atom(Str, _, N, 0, V).
eval_exp(fn('mid$'(E1, E2, E3)), V, Comp0-Comp) :- % REVIEW. E2 starts at  0/1?
  !,
  eval_exp(E1, Str, Comp0-Comp1),
  eval_exp(E2, N1,  Comp1-Comp2),
  eval_exp(E3, N2,  Comp2-Comp),
  sub_atom(Str, N1, N2, _, V).
eval_exp(fn('len'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, Str, Comp0-Comp),
  atom_length(Str, V).
eval_exp(fn('tab'(E)), V, Comp0-Comp) :-
  !,
  eval_exp(E, N, Comp0-Comp),
  string_of_char_size(V, ' ', N).
eval_exp(fn('pos'(_E)), V, Comp-Comp) :-
  !,
  get_cursor(Comp, lc(_, V)).
eval_exp(fn('csrlin'), V, Comp-Comp) :-
  !,
  get_cursor(Comp, lc(V, _)).
% Boolean expressions
eval_exp(E, V, Comp0-Comp) :-
  eval_bool(E, V, Comp0-Comp),
  !.
% User-defined functions
eval_exp(fn(F), V, Comp0-Comp) :-
  F =.. [N, E],
  udef_fn_name(N), % REVIEW. Function names are reserved?
  !,
  (get_mem(Comp0, id(N), CE-CExpr),
   copy_term(CE-CExpr, E-Expr)
   -> eval_exp(Expr, V, Comp0-Comp)
   ;  atom_concat('Undefined function ', N, ErrMsg),
      set_error('Runtime', ErrMsg, Comp0),
      !, fail).
% Functions with homologous equivalents in Prolog
eval_exp(fn(Fn), V, Comp0-Comp) :-
  equiv_fn(Fn, EFn),
  !,
  EFn =.. [EN, E],
  eval_exp(E, V1, Comp0-Comp),
  VEFn =.. [EN, V1],
  V is VEFn.
% Functions with direct arithmetic equivalents in Prolog
eval_exp(fn(F), V, Comp0-Comp) :-
  F =.. [N, E],
  !,
  eval_exp(E, EV, Comp0-Comp),
  EF =.. [N, EV],
  V is EF.


% eval_exps(+Exprs, -Values, +Comp--Comp)
eval_exps([], [], Comp-Comp).
eval_exps([E|Es], [V|Vs], Comp0-Comp) :-
  eval_exp(E, V, Comp0-Comp1),
  eval_exps(Es, Vs, Comp1-Comp).

% eval_var(+Id, -V, +Comp0--Comp)
eval_var(Id, V, Comp0-Comp) :-
  (get_mem(Comp0, Id, V)
   -> Comp = Comp0
   ;  var_default_value(Id, V), % First-time access to a variable: set a default value
      set_mem(Id, V, Comp0-Comp)).

% eval_label_exp(+LabelExp, -Label, +Comp--Comp)
eval_label_exp(LineExp-StmtExp, Line-Stmt, Comp0-Comp) :-
  eval_exp(LineExp, Line, Comp0-Comp1),
  eval_exp(StmtExp, Stmt, Comp1-Comp).


eval_bool(E1 < E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_le(Order, Result).
eval_bool(E1 <= E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_leq(Order, Result).
eval_bool(E1 > E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_ge(Order, Result).
eval_bool(E1 >= E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_geq(Order, Result).
eval_bool(E1 = E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_eq(Order, Result).
eval_bool(E1 <> E2, Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	compare(Order, V1, V2),
	eval_neq(Order, Result).
eval_bool(and(E1, E2), Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_and(V1, V2, Result).
eval_bool(or(E1, E2), Result, Comp0-Comp) :-
  eval_exp(E1, V1, Comp0-Comp1),
  eval_exp(E2, V2, Comp1-Comp),
	eval_or(V1, V2, Result).
eval_bool(not(E), Result, Comp0-Comp) :-
  eval_exp(E, V, Comp0-Comp),
	eval_not(V, Result).

eval_le(<, 1).
eval_le(=, 0).
eval_le(>, 0).

eval_leq(<, 1).
eval_leq(=, 1).
eval_leq(>, 0).

eval_eq(<, 0).
eval_eq(=, 1).
eval_eq(>, 0).

eval_neq(<, 1).
eval_neq(=, 0).
eval_neq(>, 1).

eval_ge(<, 0).
eval_ge(=, 0).
eval_ge(>, 1).

eval_geq(<, 0).
eval_geq(=, 1).
eval_geq(>, 1).

eval_and(V1, V2, Result) :-
  Result is V1 /\ V2.

eval_or(V1, V2, Result) :-
  Result is V1 \/ V2.

eval_not(V, Result) :-
  Result is \ V.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arithmetic evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% eval_int(+Int, -Integer)
% Return the integer number Integer
eval_int(Int, Int).

% eval_frac(+Int, +Frac, -Fractional)
% Return the fractional number Int.Frac
eval_frac(Int, Frac, Fractional) :-
  number_codes(Int, IntCs),
  number_codes(Frac, FracCs),
  append([IntCs, ".", FracCs], Cs),
  number_codes(Fractional, Cs).

% eval_float(+Int, +Frac, +Exp, -Float)
% Return the float number Int.Frac*10^Exp
eval_float(Int, Frac, Exp, Float) :-
  eval_frac(Int, Frac, Mantissa),
  Float is Mantissa*10^Exp.

% We use these auxiliary predicates rather than using is/2 directly
% since we might want to change the implementation of numbers
% Addition over numbers:
eval_plus(V1, V2, V) :-
  number(V1),
  number(V2),
  !,
  V is V1 + V2.
% Addition over strings (concatenation):
eval_plus(V1, V2, V) :-
  atomic_concat(V1, V2, V).

eval_sub(V1, V2, V) :-
  V is V1 - V2.

eval_mult(V1, V2, V) :-
  V is V1 * V2.

eval_div(_V1, V2, _V, Comp) :-
  V2 == 0,
  set_error('Runtime', 'Zero divisor', Comp),
  !, fail.
eval_div(V1, V2, V, _Comp) :-
  V is V1 / V2.

eval_pow(V1, V2, V) :-
  V is V1 ^ V2.

eval_sgn(V1, V) :-
  V is -V1.


% equiv_fn(?Function, ?PrologFunction)
% Prolog equivalent function to Function
equiv_fn(sqr(E), sqrt(E)).
equiv_fn(int(E), floor(E)).
equiv_fn(sgn(E), sign(E)).
equiv_fn(atn(E), atan(E)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arrays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % init_array(+Dimensions, -Array)
% init_array(Dimensions, array(Dimensions, Array)) :-
%   empty_array(Array).

% set_array(Id, Index, Value, Comp0-Comp) :-
%   (get_mem(Comp0, Id, array(Dimensions, Array0))
%    -> (length(Dimensions, L),
%        length(Index, L)
%        -> (check_array_index(Index, Dimensions)
%            -> set(Array0, Index, Value, Array),
%               set_mem(Id, array(Dimensions, Array), Comp0-Comp)
%            ;  set_error('Runtime', 'Array index out of bounds', Comp0),
%               !, fail)
%        ;  set_error('Runtime', 'Wrong number of dimensions', Comp0),
%               !, fail)
%    ;  set_error('Runtime', 'Undefined variable', Comp0),
%       !, fail).

% % check_array_index(+Index, +Dimensions)
% check_array_index([], []).
% check_array_index([I|Is], [D|Ds]) :-
%   I>=0, I<D,
%   check_array_index(Is, Ds).

% id_array_name_index(+ArrayElement, -Name, -Index)
id_array_name_index(id(ArrayElement), Name, [I0|Is]) :-
  ArrayElement =.. [Name, I0|Is].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% var_default_value(+Id, -V)
var_default_value(Id, V) :-
  (id_array_name_index(Id, Name, _Index)
   ;
   Id = id(Name)),
  !,
  (is_str_type_var(Name)
   -> V = ''
   ;  V = 0).


% set_random_state(+State)
set_random_state(State) :-
  retractall(random_state(_)),
  assertz(random_state(State)).

% Random state to recover the last random number
:- dynamic random_state/1.
:- getrand(State),
   set_random_state(State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(interpreter).

% Set of tests
% To test all of them:
%   ?- interpreter:test.
% Only a couple. Better run tests at main


% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.

test001 :-
  screen:set_screen_panel_size(4,10),
  Program = "10 let x=1",
  Result =
     comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),1,-,t,t),program:t(10-1,let(id(x),int(1)):void-void,>,t,t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,108,101,116,32,120,61,49],stack:[],status:end},
  test(basic, run, Program, Result).

% A small example program computing 4!.
test002 :-
  Program = "10 let x=1\n20 for i=1 to 4\n30 let x=x*i\n40 next i",
  Result =
    comp{cursor:lc(0,0),data:void,line:void-void,mem:t(id(x),24,<,t(id(i),5,-,t,t),t),program:t(40-1,next(id(i)):void-void,<,t(20-1,for(id(i),int(1),int(4),int(1),false):30-1,-,t(10-1,let(id(x),int(1)):20-1,-,t,t),t(30-1,let(id(x),id(x)*id(i)):40-1,-,t,t)),t(void-void,end:void-void,-,t,t)),runline:void-void,screen:'                                        ',source:[49,48,32,108,101,116,32,120,61,49,10,50,48,32,102,111,114,32,105,61,49,32,116,111,32,52,10,51,48,32,108,101,116,32,120,61,120,42,105,10,52,48,32,110,101,120,116,32,105],stack:[],status:end},
  test(basic, run, Program, Result).

