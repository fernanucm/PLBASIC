/*********************************************************/
/*                                                       */
/* LIST predicates for the BASIC language                */
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

:- module(list,
          [ interpret_list/2,
            listing_to_print_commands/3,
            format_statement/4,
            format_expr/3 ]).

:- use_module(flags,
          [ uppercase/1,
            let/1,
            optional_spaces/1 ]).

:- use_module(comp_obj).

:- use_module(screen,
          [ print_to_screen_list/2 ]).

:- use_module(misc,
          [ ceiling_line/3]).

% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% interpret_list(+Label, +Comp0--Comp)
interpret_list(Label, Comp0-Comp) :-
  listing_to_print_commands(Label, Comp0, Commands),
  print_to_screen_list(Commands, Comp0-Comp).

listing_to_print_commands(Label, Comp0, Commands) :-
  listing_to_print_commands(Label, Comp0, void, Commands).

% listing_to_print_commands(+Label, +Comp, +LastLine, -Commands)
listing_to_print_commands(void-void, _Comp, LastLine, Commands) :-
  !,
  (LastLine \== void
   -> Commands = [nl]
   ;  Commands = []).
listing_to_print_commands(Line-Stmt, Comp, LastLine, Commands) :-
  get_statement_and_next_line(Comp, Line-Stmt, Statement, NextLabel),
  format_statement(Statement, false, CodesStmt, []),
  atom_codes(FStatement, CodesStmt),
  (Line \== LastLine,
   (LastLine \== void
    -> Commands = [nl|Commands1]
    ;  Commands = Commands1)
   -> with_output_to_codes(format('~w ~w', [Line, FStatement]), Codes)
   ;  (optional_spaces(on) -> Space = ' ' ; Space = ''),
      with_output_to_codes(format('~w:~w~w', [Space, Space, FStatement]), Codes),
      Commands = Commands1),
  atom_codes(String, Codes),
  Commands1 = [val(String)|Commands2],
  skip_if_transforms(Comp, Statement, NextLabel, SkipNextLabel),
  listing_to_print_commands(SkipNextLabel, Comp, Line, Commands2).

% skip_if_transforms(+Comp, +Statement, +NextLabel, -SkipNextLabel)
% Skip the transformations of IF commands (several added lines 
%   for THEN and ELSE branches)
skip_if_transforms(_Comp, if(_, _, _, list([_],[])), NextLabel, NextLabel) :-
  !.
skip_if_transforms(Comp, if(_, _, _, _), Line-_, NextLabel) :-
  number(Line),
  !,
  Line1 is Line + 1,
  ceiling_line(Comp, Line1-1, NextLabel).
skip_if_transforms(_, _, NextLabel, NextLabel).

% format_statement(+Statement, Debug)//
% Debug is a flag with values true or false used to 
% respectively list (if) transformations or not
format_statement(run(Expr, Provided), _Debug) -->
  !,
  format_statement(run, Expr, Provided).
format_statement(list(Expr, Provided), _Debug) -->
  !,
  format_statement(list, Expr, Provided).
format_statement(dim(id(Id), Exprs), _Debug) -->
  !,
  {V =.. [Id|Exprs]},
  format_command_sp(dim),
  format_expr(id(V)).
format_statement(let(Id, Expr), _Debug) -->
  !,
  format_let,
  format_expr(Id = Expr).
format_statement(defn(Fn, Param, Name, Expr), _Debug) -->
  !,
  {copy_term([Param, Expr], [CParam, CExpr]),
   CParam = id(Name)},
  format_command_sp(def),
  format_param_assignment(Fn, [CParam], CExpr).
format_statement(rem(Remark), _Debug) -->
  !,
  format_command_sp(rem),
  format_to_codes(Remark).
format_statement(for(Id, Start, End, Step, Provided), _Debug) -->
  !,
  format_command_sp(for),
  format_expr(Id = Start),
  format_space,
  format_command_sp(to),
  format_expr(End),
  ({Provided}
   -> format_space,
      format_command_sp(step),
      format_expr(Step)
   ;  []).
format_statement(goto(LExpr-_SExpr), _Debug) -->
  !,
  format_command_exprs(goto, [LExpr]).
format_statement(goto(IntExp, GotoExps), _Debug) -->
  !,
  format_command_sp(on),
  format_expr(IntExp),
  format_space,
  format_command_sp(goto),
  format_cs_exprs(GotoExps).
format_statement(gosub(LExpr-_SExpr), _Debug) -->
  !,
  format_command_exprs(gosub, [LExpr]).
format_statement(gosub(IntExp, GosubExps), _Debug) -->
  !,
  format_command_sp(on),
  format_expr(IntExp),
  format_space,
  format_command_sp(gosub),
  format_cs_exprs(GosubExps).
format_statement(if(Cond, ThenStmt, skip, _Source), true) --> % Debug
  !,
  format_command_sp(if),
  format_expr(Cond),
  format_space,
  format_command_sp(then),
  format_statement(ThenStmt, false).
format_statement(if(Cond, ThenStmt, ElseStmt, Source), true) --> % Debug
  !,
  format_statement(if(Cond, ThenStmt, skip, Source), true),
  format_space,
  format_command_sp(else),
  format_statement(ElseStmt, false).
format_statement(if(Cond, _, _, source(ThenStmts, [])), false) --> % Screen
  !,
  format_command_sp(if),
  format_expr(Cond),
  format_space,
  format_command_sp(then),
  format_line_statements(ThenStmts).
format_statement(if(Cond, _, _, source(ThenStmts, ElseStmts)), false) --> % Screen
  !,
  format_statement(if(Cond, _, _, source(ThenStmts, [])), false),
  format_space,
  format_command_sp(else),
  format_line_statements(ElseStmts).
format_statement(print(Exprs), _Debug) -->
  !,
  format_command_exprs(print, Exprs).
format_statement(locate(LE, CE), _Debug) -->
  !,
  format_command_sp(locate),
  format_expr(LE),
  format_to_codes(','),
  format_opt_space,
  format_expr(CE).
format_statement(input(str(Msg), Variables), _Debug) -->
  !,
  format_command_sp(input),
  format_to_codes(Msg),
  format_exprs(Variables).
format_statement(data(AssocData, _), _Debug) -->
  !,
  {assoc_to_values(AssocData, Data)},
  format_command_sp(data),
  format_cs_exprs(Data).
format_statement(read(Variables), _Debug) -->
  !,
  format_command_sp(read),
  format_cs_exprs(Variables).
format_statement(restore(Expr, Provided), _Debug) -->
  !,
  format_statement(restore, Expr, Provided).
% Generic statements without arguments
format_statement(Command, _Debug) -->
  {Command =.. [CommandName],
   case(CommandName, CCommandName)},
  !,
  format_to_codes(CCommandName).
% Generic statements with one argument
format_statement(Command, _Debug) -->
  {Command =.. [CommandName, Expr]},
  !,
  format_command_exprs(CommandName, [Expr]).
% Unsupported statements (yet)
format_statement(Statement, _Debug) -->
  format_to_codes(Statement).


% format_statement(+Command, +Expr, +Provided)
% For optional argument, do not list if the user did not provide it
format_statement(Command, Expr, true) -->
  !,
  format_command_sp(Command),
  format_expr(Expr).
format_statement(Command, _Expr, false) -->
  format_command(Command).


% format_param_assignment(+Id, +Exprs, +Expr)//
% Format a parametric assignment with the form:
%   Id(I1,..,In) = Expr
format_param_assignment(Id, Exprs, Expr) -->
  {case(Id, ID),
   atomic_list_concat([ID, '('], L1)},
  format_to_codes(L1),
  format_cs_exprs(Exprs),
  format_to_codes(')'),
  format_opt_space,
  format_to_codes('='),
  format_opt_space,
  format_expr(Expr).


% format_let//
format_let -->
  {let(off)},
  !.
format_let -->
  format_command_sp(let).


% format_space//
format_space -->
  format_to_codes(' ').


% format_command_exprs(+Command, +Exprs)//
format_command_exprs(Command, Exprs) -->
  format_command(Command),
  format_space,
  format_exprs(Exprs).


% format_command(+Command)//
format_command(Command) -->
  {case(Command, CCommand)},
  format_to_codes(CCommand).


% format_command_sp(+Command)//
format_command_sp(Command) -->
  format_command(Command),
  format_space.


% format_cs_exprs(+Exprs)//
format_cs_exprs([E1]) -->
  format_expr(E1),
  !.
format_cs_exprs([E1, E2|Exprs]) -->
  format_expr(E1),
  format_to_codes(','),
  format_opt_space,
  format_cs_exprs([E2|Exprs]).


% format_line_statements(+Line-Statements)//
format_line_statements([]) -->
  [].
format_line_statements([_:Stmt]) -->
  !,
  format_statement(Stmt, false).
format_line_statements([_:Stmt1, _:Stmt2|Stmts]) -->
  format_statement(Stmt1, false),
  ":",
  format_line_statements([_:Stmt2|Stmts]).

% format_exprs(+Exprs)//
format_exprs([]) -->
  [].
format_exprs([Expr|Exprs]) -->
  format_expr(Expr),
  format_exprs(Exprs).
  
% format_expr(+Expr)//
format_expr(Expr) -->
  format_expr(Expr, 1200).

% format_expr(+Expr, +Priority)//
format_expr(int(I), _P) -->
  !,
  format_to_codes(I).
format_expr(frac(I, D), _P) -->
  !,
  {atomic_list_concat([I, '.', D], F)},
  format_to_codes(F).
format_expr(float(I, D, E), _P) -->
  !,
  {case('e', Exp),
   atomic_list_concat([I, '.', D, Exp, E], F)},
  format_to_codes(F).
format_expr(str(S), _P) -->
  !,
  format_to_codes(['"', S, '"']).
format_expr(id(Array), _P) -->
  {interpreter:id_array_name_index(id(Array), Name, Index),
   !,
   case(Name, ID)},
  format_to_codes([ID, '(']),
  format_cs_exprs(Index),
  format_to_codes(')').
format_expr(id(Id), _P) -->
  !,
  {case(Id, ID)},
  format_to_codes(ID).
format_expr(Infix, P) -->
  {Infix =.. [Op, E1, E2],
   parser:infix(Op, P1, _, _),
   case(Op, OP)},
  !,
  format_open_par(P, P1),
  format_expr(E1, P1),
  format_opt_space,
  format_to_codes(OP),
  format_opt_space,
  format_expr(E2, P1),
  format_close_par(P, P1).
format_expr(Prefix, P) -->
  {Prefix =.. [Op, E1],
   parser:prefix(Op, P1, _),
   case(Op, OP)},
  !,
  format_to_codes(OP),
  format_open_par(P, P1),
  format_expr(E1, P1),
  format_close_par(P, P1).
format_expr(Posfix, P) -->
  {Posfix =.. [Op, E1],
   parser:posfix(Op, P1, _),
   case(Op, OP)},
  !,
  format_open_par(P, P1),
  format_expr(E1, P1),
  format_close_par(P, P1),
  format_to_codes(OP).
format_expr(fn(Function), _P) -->
  {Function =.. [Fn],
   lexer:clause(function(Fn/0, _, _, _, _), _),
   !,
   case(Fn, FN)},
  format_to_codes(FN).
format_expr(fn(Function), _P) -->
  {Function =.. [Fn, Expr|Exprs],
   lexer:clause(function(Fn/Ar, _, _, _, _), _),
   length([Expr|Exprs], Ar),
   !,
   case(Fn, FN)},
  format_to_codes([FN, '(']),
  format_cs_exprs([Expr|Exprs]),
  format_to_codes(')').
format_expr(Expr, _P) -->
  format_to_codes(Expr).

% format_to_codes(+Term)//
% Convert a term or a list of terms to the code representation
format_to_codes(List) -->
  {is_list(List),
   !,
   length(List, N),
   length(Formats, N),
   maplist('='('~w'), Formats),
   atomic_list_concat(Formats, Format)},
  with_output_to_codes(format(Format, List)).
format_to_codes(Term) -->
  format_to_codes([Term]).

% format_open_par(+Priority, +FatherPriority)//
format_open_par(P, P1) -->
  format_par(P, P1, '(').

% format_close_par(+Priority, +FatherPriority)//
format_close_par(P, P1) -->
  format_par(P, P1, ')').

% format_close_par(+Priority, +FatherPriority, +Parenthesis)//
format_par(P, P1, _Par) -->
  {P >= P1,
   !}.
format_par(_P, _P1, Par) -->
  format_to_codes(Par).

% format_opt_space//
format_opt_space -->
  {optional_spaces(off),
   !}.
format_opt_space -->
  format_space.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% case(+X, -CX)
case(X, CX) :-
  uppercase(off),
  !,
  downcase_atom(X, CX).
case(X, CX) :-
  upcase_atom(X, CX).


