/*********************************************************/
/*                                                       */
/* PARSER for the BASIC language                         */
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

:- module(parser,
          [ parse/2,
            parse_transform/2,
            transform/2,
            expr/3,
            is_number/1 ]).

:- use_module(misc).

:- use_module(test,
          [ test/4 ]).

:- use_module(error_,
          [ set_error/3,
            set_error/4,
            reset_error/0,
            process_error/0 ]).


% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).

% The operator # is used to couple the consumed token with 
%   the error message corresponding to the expected token.
%   This error message includes the position
:- op(995, xfy, #).


% parse(+Tokens, -SyntaxTree) is det
% Main predicate of this module
% Parse the tokens from a BASIC programa and return the corresponding list of sentences, each one represented by its syntax tree 
% If parse/2 fails, an error should be asserted already,
% and then it is displayed with process_error/0
% Currently, this is omitted and handled by the caller to parse/2
parse(Tokens, SyntaxTrees) :-
  reset_error,
  phrase(parse(SyntaxTrees/[]), Tokens),
  !.
% parse(_Tokens, _SyntaxTrees) :-
%   process_error,
%   !, fail.

% parse_transform(+Tokens, -SyntaxTree) is det
parse_transform(Tokens, SyntaxTrees) :-
  parse(Tokens, SyntaxTrees0),
  transform(SyntaxTrees0, SyntaxTrees).

% transform(+SyntaxTrees0, -SyntaxTrees) is det
transform(SyntaxTrees0, SyntaxTrees) :-
  phrase(transform_ifs(SyntaxTrees/[]), SyntaxTrees0),
  !.
transform(_SyntaxTrees0, _SyntaxTrees) :-
  format('Unexpected Error: Transform error after parsing'),
  !, fail.

% parse(-STs1/STs)//
parse(STs1/STs) -->
  newline_star,
  program_line(STs1/STs2),
  !,
  rest_of_lines(STs2/STs).
parse(STs/STs) --> % Empty program
  newline_star.
  
% rest_of_lines(-STs1/STs)//
rest_of_lines(STs1/STs) -->
  newlines,
  program_line(STs1/STs2),
  !,
  rest_of_lines(STs2/STs).
rest_of_lines(STs/STs) -->
  newline_star.

% newlines//
% One or more new lines
newlines -->
  newline,
  newline_star.

% newline//
% One new line
newline -->
  [punct(nl):_Pos].

% newline_star//
% Zero or more new lines
newline_star -->
  newlines
  -> !
  ;  [].

% skip_line//
% Consume input up to end of line
skip_line, [punct(nl):Pos] -->
  [punct(nl):Pos],
  !.
skip_line -->
  [_],
  !,
  skip_line.
skip_line -->
  []. % EOF

% program_line(-STs1/STs)//
program_line(STs1/STs) -->
  current_position(Position),
  int(Line)                 # 'Line number',
  {(Line >=0, Line < 65530
    -> true
    ;  set_error('Semantic', 'Line numbers must be between 0 and 65529', Position), 
       !, fail)},
  statements(STs1/STs, Line-0-_).

% statements(-STs1/STs, +Line-+StmtIn--StmtOut)//
% The second argument (L-N-N1) are base 1 for line, and input and output
% number of the statement in the line
% At least, one statement
statements(STs1/STs, Line-N0-N) -->
  {N1 is N0+1},
  statement(STs1/STs2, Line-N1-N2)    # 'Statement',
  ([punct(':'):_Pos]                  # 'Colon'
   -> !,
      statements(STs2/STs, Line-N2-N) # 'Statement'
   ;  {N = N2,
       STs2 = STs}).

% statement(-STs1/STs, +Line-+StmtIn--StmtOut)//
statement([L-N:run(Line, Prov)|STs]/STs, L-N-N) -->  % Go to the first statement at line Line
  cmd(run)                            # 'Statement',
  optional_line_number(Line, Prov)    # 'Line number',
  !.
statement([L-N:list(Line, Prov)|STs]/STs, L-N-N) -->  % List the program from the optional given line
  cmd(list)                           # 'Statement',
  optional_line_number(Line, Prov)    # 'Line number',
  !.
statement([L-N:new|STs]/STs, L-N-N) -->
  cmd(new)                            # 'Statement',
  !.
statement([L-N:clear|STs]/STs, L-N-N) -->
  cmd(clear)                          # 'Statement',
  !.
statement([L-N:cont|STs]/STs, L-N-N) -->
  cmd(cont)                           # 'Statement',
  !.
statement([L-N:dim(id(Id), Dimensions)|STs]/STs, L-N-N) -->
  cmd(dim)                            # 'Statement',
  non_array_variable(Id)              # 'variable',
  punct('(')                          # 'Opening parenthesis',
  dim_dimensions(Dimensions)          # 'Dimension of the array',
  punct(')')                          # 'Closing parenthesis',
  !,
  ({length(Dimensions, DL), DL > 5}
    -> set_error('Semantic', 'Too many dimensions (max. 5)')
    ;  {true}).
% statement([L-N:let(id(Id), Index, Expr)|STs]/STs, L-N-N) -->
%   optional_cmd(let)                   # 'Statement',
%   array_variable(Id, Index)           # 'Variable',
%   op('=')                             # 'Equal symbol (=)',
%   expr(Expr)                          # 'Expression',
%   !.
statement([L-N:let(id(Id), Expr)|STs]/STs, L-N-N) -->
  optional_cmd(let)                   # 'Statement',
  variable(Id)                        # 'Variable',
  op('=')                             # 'Equal symbol (=)',
  expr(Expr)                          # 'Expression',
  !.
statement([L-N:defn(Fn, Param, Id, RExpr)|STs]/STs, L-N-N) -->
  cmd(def)                            # 'Statement',
  fn(Fn/_)                            # 'Function name',
  {udef_fn_name(Fn)},
  punct('(')                          # 'Opening parenthesis',
  id(Id)                              # 'Numeric variable',
  punct(')')                          # 'Closing parenthesis',
  op('=')                             # 'Equal symbol (=)',
  num_expression(Expr)                # 'Numeric expression',
  !,
  {replace_term(id(Id), Param, Expr, RExpr)}.
statement([L-N:cls|STs]/STs, L-N-N) -->
  cmd(cls)                            # 'Statement',
  !.
statement([L-N:beep|STs]/STs, L-N-N) -->
  cmd(beep)                           # 'Statement',
  !.
statement([L-N:rem(Remark)|STs]/STs, L-N-N) --> 
  cmd(rem(Remark))                    # 'Statement',
  !,
  skip_line.
statement([L-N:for(id(Id), Start, End, Step, OptStep)|STs]/STs, L-N-N) -->
  cmd(for)                            # 'Statement',
  id(Id)                              # 'Integer variable',
  op('=')                             # 'Equal symbol (=)',
  num_expression(Start)               # 'Numeric expression',
  cmd(to)                             # 'Keyword TO', 
  num_expression(End)                 # 'Numeric expression',
  optional_step(Step, OptStep)        # 'Optional keyword STEP',
  !.
statement([L-N:next(id(Id))|STs]/STs, L-N-N) -->
  cmd(next)                           # 'Statement',
  id(Id)                              # 'Integer variable',
  !.
statement([L-N:goto(Line)|STs]/STs, L-N-N) -->  % Go to the first statement at line Line
  cmd(goto)                           # 'Statement',
  !,
  line(Line).
statement([L-N:goto(Int, Lines)|STs]/STs, L-N-N) --> 
  cmd(on)                             # 'Statement',
  int_expression(Int)                 # 'Integer expression',
  cmd(goto)                           # 'Keyword GOTO or GOSUB',
  !,
  cs_nat_exprs(Lines).
statement([L-N:gosub(Line)|STs]/STs, L-N-N) -->  % Go to the subroutine located at the first statement at line Line
  cmd(gosub)                          # 'Statement',
  !,
  line(Line).
statement([L-N:gosub(Int, Lines)|STs]/STs, L-N-N) --> 
  cmd(on)                             # 'Statement',
  int_expression(Int)                 # 'Integer expression',
  cmd(gosub)                          # 'Keyword GOTO or GOSUB',
  !,
  cs_nat_exprs(Lines).
statement([L-N:return|STs]/STs, L-N-N) -->  % Return from the subroutine
  cmd(return)                         # 'Statement',
  !.
statement([L-N0:If|STs]/STs, L-N0-N) -->
  cmd(if)                             # 'Statement',
  bool_expression(Cond)               # 'Boolean expression',
  cmd(then)                           # 'Keyword THEN',
  then_else_statements(If, Cond, L-N0-N),
  !.
statement([L-N:print(Exprs)|STs]/STs, L-N-N) -->
  cmd(print)                          # 'Statement',
  print_exprs(Exprs)                  # 'Expression',
  !.
statement([L-N:locate(Line, Col)|STs]/STs, L-N-N) --> % REVIEW order of arguments
  cmd(locate)                         # 'Statement',
  int_expression(Line)                # 'Integer expression',
  punct(',')                          # 'Comma',
  int_expression(Col)                 # 'Integer expression',
  !.
statement([L-N:input(MsgExpr, Variables)|STs]/STs, L-N-N) -->
  cmd(input)                          # 'Statement',
  optional_input_message(MsgExpr),
  cs_variables(Variables)             # 'Variables',
  !.
statement([L-N:data(Data, _Next)|STs]/STs, L-N-N) -->
  cmd(data)                           # 'Statement',
  cs_values(Values)                   # 'Values',
  !,
  {length(Values, M),
   findall(I-V, (between(1, M, I), nth1(I, Values, V)), Pairs),
   list_to_assoc(Pairs, Data)}.
statement([L-N:read(Variables)|STs]/STs, L-N-N) -->
  cmd(read)                           # 'Statement',
  cs_variables(Variables)             # 'Variables',
  !.
statement([L-N:restore(Line, Prov)|STs]/STs, L-N-N) -->
  cmd(restore)                        # 'Statement',
  optional_line_number(Line, Prov)    # 'Line number'.
statement([L-N:stop|STs]/STs, L-N-N) -->
  cmd(stop)                           # 'Statement',
  !.
statement([L-N:end|STs]/STs, L-N-N) -->
  cmd(end)                            # 'Statement',
  !.
% Unsupported commands in Seiko Data 2000
statement([L-N:pause(Seconds)|STs]/STs, L-N-N) -->
  cmd(pause)                            # 'Statement',
  !,
  num_expression(Seconds).


% optional_input_message(-Expr)//
% For the INPUT command
optional_input_message(Expr) -->
  str_expression(Expr)                # 'Optional expression',
  punct(';')                          # 'Semicolon',
  !.
optional_input_message(str('')) -->
  [].


% optional_line_number(-Line, -Provided)//
% For RUN, RESTORE, LIST commands
optional_line_number(Line, true) -->
  nat_expression(Line),
  !.
optional_line_number(int(1), false) -->
  [].


% line(-Line)//
line(Line) -->
  nat_expression(Int)                 # 'Line number or expression',
  {goto_label(Int, Line)}.


% cs_nat_exprs(-Ns)//
% Comma-separated naturals (0..)
cs_nat_exprs([N1, N2|Ns]) -->
  nat_expression(N1)                  # 'Natural expression',
  punct(',')                          # 'Comma',
  !,
  cs_nat_exprs([N2|Ns]).
cs_nat_exprs([N]) -->
  nat_expression(N)                   # 'Natural expression'.


% cs_variables(-Vs)//
% Comma-separated atomic variables
cs_variables([id(V1), V2|Vs]) -->
  variable(V1)                        # 'Variable',
  punct(',')                          # 'Comma',
  !,
  cs_variables([V2|Vs]).
cs_variables([id(V)]) -->
  variable(V)                         # 'Variable'.


% cs_values(-Vs)//
% Comma-separated numbers and strings
cs_values([V1, V2|Vs]) -->
  value(V1)                           # 'Value (number or string)',
  punct(',')                          # 'Comma',
  !,
  cs_values([V2|Vs]).
cs_values([V]) -->
  value(V)                            # 'Value (number or string)'.

% value(-Value)//
value(int(I)) -->
  int(I),
  !.
value(frac(I, F)) -->
  frac(I, F),
  !.
value(float(I, F, Ex)) -->
  float(I, F, Ex),
  !.
value(str(Str)) -->
  [str(Str):_],
  !.


% is_number(+Expr)
% Succeed if Expr is a number
is_number(Expr) :-
  value(_, [Expr:_], []),
  Expr \= str(_).


% dim_dimensions(-Dimensions)//
% Dimensions of a DIM statement
dim_dimensions([Dim1, Dim2|Dimensions]) -->
  dim_dimensions([Dim1])              # 'Integer expression',
  punct(',')                          # 'Comma',
  dim_dimensions([Dim2|Dimensions]).
dim_dimensions([Dim]) -->
  int_expression(Dim)                 # 'Integer expression'.


% print_exprs(-Exprs)//
print_exprs([Expr, ';'|Exprs]) -->
  expr(Expr)                          # 'Expression',
  punct(';')                          # 'Semicolon',
  !,
  print_exprs(Exprs).
print_exprs([Expr1, ',', Expr2|Exprs]) -->
  expr(Expr1)                         # 'Expression',
  punct(',')                          # 'Comma',
  !,
  print_exprs([Expr2|Exprs]).
% print_exprs([Expr1, ';', Expr2|Exprs]) --> % Two expressions without separator! Separators should be kept in the lexer to really know if there is no one here
%   expr(Expr1)                         # 'Expression',
%   expr(Expr2)                         # 'Expression',
%   !,
%   print_exprs(Exprs).
print_exprs([Expr]) -->
  expr(Expr)                          # 'Expression'.
print_exprs([]) -->
  [].


% then_else_statements(-If, -Cond, +L-+N0--N)//
then_else_statements(If, Cond, L-N0-N) -->
  branch_statements(TSTs0/TSTs1, L-N0-N1),
  ((cmd(else)                         # 'Keyword ELSE')
   -> %{N2 is N1 + 1},
      branch_statements(ESTs0/ESTs1, L-N1-N)
   ;  {ESTs1 = ESTs0, N = N1}),
  {build_if_then_else(If, Cond, TSTs0/TSTs1, ESTs0/ESTs1)}.

% branch_statements(-STs0/STs, +L-+N0--N)//
% IF..THEN integer:stmt... 'integer' must be the first statement, if present
% Other statements, though allowed, are never executed
branch_statements([L-N1:goto(Addr)|STs0]/STs, L-N0-N) --> 
  int(I)                              # 'Line number',
  !,
  {goto_label(int(I), Addr),
   N1 is N0 + 1},
  ( [punct(':'):_Pos]                 # 'Colon',
    statements(STs0/STs, L-N1-N) 
  ; {STs0 = STs, N = N1} ).
% IF..THEN stmt1:stmt2...
branch_statements(STs0/STs, L-N0-N) -->
  statements(STs0/STs, L-N0-N).

% build_if_then_else(-If, +Cond, ?ThenStms, ?ElseStmts)
% If with no Else
build_if_then_else(if(Cond, TSTs0, [], source(TSTs0, [])), Cond, TSTs0/TSTs, ESTs0/ESTs) :-
  ESTs0 == ESTs,
  !,
  TSTs = [].
% If with Else
build_if_then_else(if(Cond, TSTs0, ESTs0, source(TSTs0, ESTs0)), Cond, TSTs0/TSTs, ESTs0/ESTs) :-
  TSTs = [],
  ESTs = [].


% optional_step(Step)//
% Optional STEP in the FOR command
optional_step(Step, true) -->
  cmd(step)                           # 'Keyword STEP',
  num_expression(Step)                # 'Numeric expression',
  !.
optional_step(int(1), false) -->
  [].


% num_expression(-Expression)//
num_expression(Expression) -->
  expr(Expression),
  !,
  {is_num_expression(Expression)}.

% int_expression(-Expression)//
int_expression(Expression) -->
  expr(Expression),
  !,
  {is_int_expression(Expression)}.

% nat_expression(-Expression)//
nat_expression(Expression) -->
  expr(Expression),
  !,
  {is_nat_expression(Expression)}.

% bool_expression(-Expression)//
bool_expression(Expression) -->
  expr(Expression),
  !,
  {is_bool_expression(Expression)}.

% str_expression(-Expression)//
str_expression(Expression) -->
  expr(Expression),
  !,
  {is_str_expression(Expression)}.

% is_num_expression(+Expression)
is_num_expression(Expression) :- % REFINE. Naive test
  Expression \= str(_).

% is_str_expression(+Expression)
is_str_expression(Expression) :- % REFINE. Naive test
  Expression \= int(_),
  Expression \= frac(_, _),
  Expression \= float(_, _, _).

% is_bool_expression(+Expression)
is_bool_expression(Expression) :- % REFINE. Naive test
  is_int_expression(Expression).

% is_int_expression(+Expression)
is_int_expression(Expression) :-  % REFINE. Naive test
  Expression \= str(_),
  Expression \= frac(_, _),
  Expression \= float(_, _, _).

% is_nat_expression(+Expression)
is_nat_expression(Expression) :- % REFINE. Naive test
  is_int_expression(Expression).


% VALUES

% int(-I)//
int(I) -->
  [int(I):_Pos].

% frac(I, F)//
frac(I, F) -->
  [frac(I, F):_Pos].

% float(I, F, E)//
float(I, F, E) -->
  [float(I, F, E):_Pos].


% VARIABLES

% variable(-Id)//
% Variable (e.g., a, a$) or array element (e.g., a(0), a$(0))
variable(Id) -->
  array_variable(Id)           # 'Variable',
  !.
variable(Id) -->
  non_array_variable(Id)       # 'Variable'.

% non_array_variable(-Id)//
non_array_variable(Id) -->
    numeric_variable(Id)
  ; string_variable(Id).

% numeric_variable(-Id)//
% A function, operator or command name can play the role of a numeric
% variable (no reserved words), but not a string variable (ended in '$')
numeric_variable(Id) -->
    [id(Id):_],
    {\+ is_str_type_var(Id)}
  ; [fn(Id/_Ar):_]
  ; [cmd(Id):_]
  ; [op(Id):_].

% string_variable(-Id)//
string_variable(Id) -->
    [id(Id):_],
    {is_str_type_var(Id)}.

% array_variable(-Id, -Index)//
% Name of the array variable and its index as a list of expressions
array_variable(Id) -->
  non_array_variable(N)               # 'Variable',
  punct('(')                          # 'Opening parenthesis',
  cs_nat_exprs(Index)                 # 'Array index',
  punct(')')                          # 'Closing parenthesis',
  {Id =.. [N|Index]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORM IF's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An if-then-else can generate new lines.
% Instructions are labeled with L-S, standing for Line and 
% statement numbers respectively (base 1)
% Cases:
% 1) The general form is:
%   if cond then Tstmt1:..:TstmtN else Estmt1:..:EstmtM
%    represented as:
%   L-S : if(Cond, [L-(S+1)  :Tstmt1,..,L-(S+N)  :TstmtN],
%                  [L-(S+N+1):Estmt1,..,L-(S+N+M):EstmtM])
% is translated into:
%   L-S  : if(Cond, skip, goto(Estmt1), List)
%   L-S+1: Tstmt1
%   ...
%   L-S+N: TstmtN
%   L-S+N+1: goto(next_line(EstmtM))
%   L-S+N+2: Estmt1
%   ...
%   L-S+M+1: EstmtM
% 2) If there is only the THEN branch and several Tstmt's:
%   If cond then Tstmt1:..:TstmtN
%    represented as:
%   L-S : if(Cond, [L-(S+1):Tstmt1,..,L-(S+N):TstmtN], [])
% is translated into:
%   L-S  : if(Cond, skip, goto(next_line(TstmtN), List)
%   L-S+1: Tstmt1
%   ...
%   L-S+N: TstmtN
% 3) If there is only the THEN branch and only one Tstmt
%   If cond then Tstmt
%    represented as:
%   L-S : if(Cond, [L-(S+1):Tstmt], [])
% is translated into:
% L-S  : if(Cond, Tstmt, skip, List)
% NOTE: List includes the original THEN and ELSE branches which
%       are kept for showing the original formulation in listings
%       as the term source([Tstmt1,..,TstmtN], [Estmt1,..,EstmtM])

transform_ifs(STs/STs) -->
  [].
% Case 3) Only one statement in THEN
transform_ifs([LIf:if(Cond, ThenStmt, skip, Source)|STs0]/STs) -->
  [LIf:if(Cond, [_LT:ThenStmt], [], Source)],
  !,
  transform_ifs(STs0/STs).
% Case 2) Several statements in THEN
transform_ifs([LIf:if(Cond, skip, goto(LabelExp), Source)|STs0]/STs) -->
  [LIf:if(Cond, ThenStmts, [], Source)],
  !,
  (state(S)
    -> { S = Label:_ }
    ;  { Label = void-void }),
  {append(ThenStmts, STs1, STs0),
   label_exp(Label, LabelExp)},
  transform_ifs(STs1/STs).
% Case 1) THEN and ELSE
transform_ifs([Label:if(Cond, skip, goto(ElseLabel1Exp), Source)|STs0]/STs) -->
  [Label:if(Cond, ThenStmts, ElseStmts, Source)],
  !,
  (state(S)
    -> { S = EndLabel:_ }
    ;  { EndLabel = void-void }),
  {ElseStmts = [ElseLabel:_|_],
   ElseLabel = ElseLine-ElseStmt,
   ElseStmt1 is ElseStmt + 1,
   ElseLabel1 = ElseLine-ElseStmt1,
   label_exp(ElseLabel1, ElseLabel1Exp),
   label_exp(EndLabel, EndLabelExp),
   append(ThenStmts, [ElseLabel:goto(EndLabelExp)|STs1], STs0),
   else_statements(ElseStmts, ElseStmt1, STs1/STs2)},
  transform_ifs(STs2/STs).
% Non-If statements: do nothing
transform_ifs([ST|STs0]/STs) -->
  [ST],
  transform_ifs(STs0/STs).

% else_statements(+ElseStmts, +N, +STs1/-STs2)
else_statements([], _N, STs/STs).
else_statements([L-_:ElseStmt|ElseStmts], N, [L-N:ElseStmt|STs0]/STs) :-
  N1 is N + 1,
  else_statements(ElseStmts, N1, STs0/STs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adapted from: "The simple and powerful yfx operator precedence parser", E. L. Favero, 2007, Softw. Pract. Exper., 37:1451-1474, Wiley

% expr(-Expression)//
expr(E) -->
  % Start with the lowest precedence (1 is the maximum precedence)
  expr(E, 1200),
  !.
  
% expr(-Expression, +Precedence)//
% Op: Operator
% LP: Left Precedence, RP: Right Precedence, PP: Principal Precedence
expr(E, PP) --> % Integers/Fractionals/Floats/Strings
  value(V),
  !,
  rExpr(V, E, 0, PP).
expr(E, PP) --> % Parenthesized expressions
  [punct('('):_],
  expr(TI),
  [punct(')'):_],
  !,
  rExpr(TI, E, 0, PP).
expr(E, PP) --> % 0-Arity Functions
  [fn(N/0):_],
  !,
  rExpr(fn(N), E, 0, PP).
expr(E, PP) --> % Functions
  [fn(N/Ar):_,
   punct('('):_],
  fn_args(Ar, As),
  [punct(')'):_],
  !,
  {Fn =.. [N|As]},
  rExpr(fn(Fn), E, 0, PP).
expr(E, PP) --> % Prefix operators (no posfix in BASIC)
  [op(Op):_],
  {prefix(Op, P, RP),
   P =< PP,
   !},
   %true},
  expr(Arg, RP),
  {NE =.. [Op, Arg]},
  rExpr(NE, E, P, PP).
expr(E, PP) --> % Array element
  non_array_variable(N),
  [punct('('):_],
  cs_nat_exprs(Is), % Index
  [punct(')'):_],
  !,
  {Id =.. [N|Is]},
  rExpr(id(Id), E, 0, PP).
expr(E, PP) --> % Variables (numeric and strings)
  non_array_variable(Id),
  !,
  rExpr(id(Id), E, 0, PP).

% rExpr(+Expr, -E, +LeftP, +PP)//
rExpr(Expr, E, LeftP, PP) -->
  [op(Op):_],
  {infix(Op, P, LP, RP),
   P =< PP, 
   LeftP =< LP,
   (redef(Op) -> true ; !)},
   %(redef(Op) -> true ; true)},
  expr(Arg2, RP),
  {NE =.. [Op, Expr, Arg2]},
  rExpr(NE, E, P, PP).
rExpr(Expr, E, LeftP, PP) -->
  [op(Op):_],
  {posfix(Op, P, LP),
   P =< PP,
   LeftP =< LP,
   (redef(Op) -> true ; !),
   NE =.. [Op, Expr]},
  rExpr(NE, E, P, PP).
rExpr(E, E, _, _) -->
  [].


% fn_args(+Arity, -As)//
% Function arguments
fn_args(Ar, H) -->
  expr(A1),
  ([punct(','):_]
   -> {H = [A1, A2|As],
       Ar1 is Ar - 1},
      fn_args(Ar1, [A2|As])
   ;  {H = [A1],
       Ar == 1}).


% Operators
% prefix(?Op, ?P1, ?P2).
%   prefix(P, Op, P-1) :- op(P, fx, Op).
%   prefix(P, Op, P)   :- op(P, fy, Op).
prefix(+,   380, 380).
prefix(-,   380, 380).
prefix(not, 150, 150).

% infix(?Op, ?P1, ?P2, ?P3).
%   infix(P, Op, P-1, P-1)  :- op(P, xfx, Op).
infix(=,  700, 699, 699).
infix(>=, 700, 699, 699).
infix(<=, 700, 699, 699).
infix(<>, 700, 699, 699).
infix(>,  700, 699, 699).
infix(<,  700, 699, 699).

% infix(P, Op, P-1, P)    :- op(P, xfy, Op).
infix(^, 200, 199, 200).

% infix(P, Op, P,   P-1)  :- op(P, yfx, Op).
infix(or,  800, 800, 799).
infix(and, 800, 800, 799).
infix(+,   500, 500, 499).
infix(-,   500, 500, 499).
infix(*,   400, 400, 399).
infix(/,   400, 400, 399).

% posfix(?Op, ?P1, ?P2).
%   posfix(P, Op, P-1) :- op(P, xf, Op).
%   posfix(P, Op, P)   :- op(P, yf, Op).
% Example (though unsupported in Seiko Data 2000): 
posfix(!, 280, 280).

% redef(?Op)
% Redefined operators
redef(+).
redef(-).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxilliary predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% +Token # +Error//
% Set errors for expected tokens
Token # Error -->
  {terminal(Token),
   !},
  [CurrentToken:Position],
  {set_error('Syntax', Error, Position),
   Token = CurrentToken},
  !.
Goal # Error -->
  current_position(Position),
  {set_error('Syntax', Error, Position)},
  Goal.


% optional_cmd(-Cmd)//
% Optional command
optional_cmd(Cmd) -->
  ([cmd(Cmd):_]
   -> !
   ;  []).


% terminal(?Token)
terminal(id(_)).
terminal(cmd(_)).
terminal(op(_)).
terminal(fn(_)).
terminal(int(_)).
terminal(frac(_, _)).
terminal(float(_, _, _)).
terminal(str(_)).
terminal(punct(_)).


% goto_label(+Int, -Int-int(1))
goto_label(Int, Int-int(1)).


% replace_term(+Subterm0, +Subterm, +Term0, -Term)
% Return in Term the replacement of Subterm0 by Subterm in Term0
replace_term(Subterm0, Subterm, Term0, Term) :-
  ( Term0 == Subterm0 -> Term = Subterm
  ; var(Term0) -> Term = Term0
  ; Term0 =.. [F|Args0],
    maplist(replace_term(Subterm0,Subterm), Args0, Args),
    Term =.. [F|Args] ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(parser).

% Set of tests
% To test all of them: 
%   ?- parser:test.

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.

test001 :-
  test(parser, parse, [int(10):pos(1,1),cmd(for):pos(1,1),id(i):pos(1,5),op(=):pos(1,6),int(1):pos(1,7),cmd(to):pos(1,9),id(n):pos(1,12),cmd(step):pos(1,14),int(-1):pos(1,19)],[10-1:for(id(i),int(1),id(n),int(-1),true)]).

test002 :-
  test(parser, parse, 
  [int(10):pos(1,1),cmd(let):pos(1,3),id(sin):pos(1,10),op(=):pos(1,13),fn(sin/1):pos(1,7),punct('('):pos(1,13),int(1):pos(1,1),punct(')'):pos(1,13)], [10-1:let(id(sin),fn(sin(int(1))))]).

test003 :-
  test(parser, parse, [int(10):pos(1, 1), cmd(print):pos(1, 4), fn(sin/1):pos(1, 10), punct('('):pos(1, 13), int(1):pos(1, 14), punct(')'):pos(1, 15)], [10-1:print([fn(sin(int(1)))])]).


