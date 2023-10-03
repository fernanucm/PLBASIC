/*********************************************************/
/*                                                       */
/* LEXER for the BASIC language                          */
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

:- module(lexer,
          [ lex/2 ]).

:- use_module(misc,
          [ udef_fn_name/1 ]).

:- use_module(test,
          [ test/4 ]).

:- use_module(library(edcg)). % For multiple DCG accumulators

:- use_module(error_,
          [ set_error/3,
            reset_error/0,
            process_error/0 ]).

% This SWI-Prolog flag makes strings delimited by double 
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).


%%%% Extended DCG declarations
% Cf. Peter Van Roy's paper (https://github.com/kamahen/edcg/blob/master/docs/CSD-90-583.pdf)

% Accumulators (in addition to the predefined 'dcg')
edcg:acc_info(position, X, In, Out, acc_pos(X, In, Out)).

% Declare predicates using these hidden arguments
% pred_info(+Predicate, +Arity, +List_of_accumulators)
% 'dcg' represents the usual DCG accumulator.
% The additional 'position' accumulator is used to
% accumulate the term pos(Line, Column).

edcg:pred_info(Name, _, [position, dcg]) :-
  \+ memberchk(Name, ['!',           % Do not expand
                      fail,          % "   "   "
                      eoc,           % Only 'dcg' accumulator
                      kw,            % "     "
                      inc_line,      % Only 'position' accumulator
                      inc_col,       % "     "
                      add_col,       % "     "
                      get_pos        % "     "
                      ]).

edcg:pred_info(eoc,           0, [dcg]).
edcg:pred_info(kw,            1, [dcg]).

edcg:pred_info(inc_line, 0, [position]).
edcg:pred_info(inc_col,  0, [position]).
edcg:pred_info(add_col,  1, [position]).
edcg:pred_info(get_pos,  1, [position]).

%%%% End of extended DCG declarations

% Lexical categories:
%
% - (User) Identifiers:
%     id(Identifier). For numbers and strings
% - Commands:
%     cmd(Command)
% - Functions
%     fn(Function/Arity)
% - Operators (symbolic and textual):
%     op(Operator)
% - Numbers
%     int(Number). Integer
%     frac(Integer, Fractional). Fractional
%     float(Integer, Fractional, Exponent). Float
% - Strings (delimited by double quotes)
%     str(String)
% - Punctuation: ( ) , ; : "

% lex(+Input, -Tokens)
% Main predicate of this module
% Read tokens from Input, which can be either a file (an atom)
%   or a list of codes
% All codes from Input are read in memory at once
%   (if memory is an issue, read_line_to_codes/3 can
%    be used instead for a language such as BASIC,
%    where each line is a statement or a program line).
% Encoding is as set by the Prolog flag encoding 
%   (it can nevertheless be specified as an option 
%    of read_file_to_codes/3, as well as the locale).
% If lex/2 fails, an error should be asserted already,
% and then it is displayed
% Currently, this is omitted and handled by the caller to lex/2
lex(Input, Tokens) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens).
  
lex_codes(Codes, Tokens) :-
  token_pos_list(Tokens, pos(1,1), _Pos, Codes, []),
  !.
% lex_codes(_Codes, _Tokens) :-
%   process_error,
%   !, fail.

% token_pos_list(-TokensPosList)// 
%   +InPosition:position, -OutPosition:position, ?Codes:dcg, ?TailCodes:dcg
% Get tokens and their positions (pos(Line, Column)) from Codes, with 
%   tail codes in TailCodes, and return the new position in the
%   text.
token_pos_list(TokenPosList) -->>
  separators_star,
  get_pos(TokPos):position,
  token(Token),
  get_pos(NxtPos):position,
  separator(Token, NextToken),
  !,
  {(NextToken == no
   -> TokenPosList = [Token:TokPos|RemainingTokenPosList]
   ;  TokenPosList = [Token:TokPos, NextToken:NxtPos|RemainingTokenPosList])},
  token_pos_list(RemainingTokenPosList).
token_pos_list([]) -->>
  separators_star.


% separator(+Token, -DelimiterToken)//
% Determines if a separator is needed. If the next token is 
%   such a separator, it is consumed and returned; otherwise, 
%   is is marked as 'no' (separator).

% If there are no more codes to read, there is no need for 
%   a final separator
separator(_Token, no) -->>
  eoc,
  !.
% If the previous token is an operator, there is no need for a separator
separator(op(_), no) -->>
  !,
  [].
% If the previous token is a punctuation mark, there is no need for a separator
separator(punct(_), no) -->>
  !,
  [].
% If the previous token is a string, there is no need for a separator
separator(str(_), no) -->>
  !,
  [].
% If next codes are a string, there is no need for a separator
separator(_Token, String) -->>
  string(String),
  !.
% If next code is a newline, there is no need for a separator
separator(_Token, punct(nl)) -->>
  "\n",
  !,
  inc_line.
% If next codes are a delimiter mark, there is no need for a separator
separator(_Token, Delimiter) -->>
  delimiter(Delimiter),
  !.
% Otherwise, a separator is needed
separator(_Token, no) -->>
  separator.

% separators//
% One or more separators.
separators -->>
  skip_non_visible,
  separator,
  !,
  separators_star.

% separators_star//: Zero or more separators.
separators_star -->>
  separators,
  !.
separators_star -->>
  skip_non_visible,
  [].

% separator//
% One separator (blank, tabulator, end of file)
separator -->>
  " ",
  inc_col:position,
  !.
separator -->>
  "\t",
  !.
separator -->>
  "end_of_file",
  !.


% skip_non_visible//
% Skip non-visible characters
skip_non_visible -->>
  [C],
  {non_visible_code(C)},
  !,
  skip_non_visible.
skip_non_visible -->>
  [].
  

% token(-Token)//
% Tokens in the language
token(Number) -->>
  number(Number),
  !.
token(String) -->>
  string(String),
  !. 
token(Delimiter) -->>
  delimiter(Delimiter),
  {Delimiter \== punct('"')}, % A double quote cannot occur out of a string
  !.
token(cmd(rem(Remark))) -->> % Special command REM: include the rest of the line as the remark
  command(rem),
  !,
  remark(Remark).
token(cmd(Command)) -->>
  command(Command),
  !.
token(fn(Function)) -->>
  function(Function),
  !.
token(op(Operator)) -->>
  textual_operator(Operator),
  !.
token(id(Identifier)) -->>
  identifier(Identifier),
  !.
token(_Error) -->>
  set_error(token),
  !, fail.


% remark(-Remark)//
remark(Remark) -->>
  remark_codes(Codes0),
  {(append([32|_], Codes, Codes0) % Remove the first blank, if it exists
    -> true
    ;  Codes = Codes0),
   atom_codes(Remark, Codes)}.

% remark_codes(-Remark)//
remark_codes([]) -->>
  dcg/[10|_], % Lookahead end of line
  !.
remark_codes([Code|Codes]) -->>
  [Code],
  inc_col,
  !,
  remark_codes(Codes).
remark_codes([]) -->> % No more codes are left to read
  [],
  !.


% delimiter(-Delimiter)//
% Symbolic operators and punctuation marks delimit tokens
delimiter(op(Delimiter)) -->>
  operator(Delimiter),
  !.
delimiter(punct(Delimiter)) -->>
  punctuation(Delimiter).


% operator(-Operator)//
% Operators
operator('=')   -->> "=",   !, inc_col.
operator('>=')  -->> ">=",  !, add_col(2).
operator('<=')  -->> "<=",  !, add_col(2).
operator('<>')  -->> "<>",  !, add_col(2).
operator('>')   -->> ">",   !, inc_col.
operator('<')   -->> "<",   !, inc_col.
operator('+')   -->> "+",   !, inc_col.
operator('-')   -->> "-",   !, inc_col.
operator('*')   -->> "*",   !, inc_col.
operator('/')   -->> "/",   !, inc_col.
operator('^')   -->> "^",   !, inc_col.

textual_operator('and') -->> lc("and"), !, add_col(3).
textual_operator('or')  -->> lc("or"),  !, add_col(2).
textual_operator('not') -->> lc("not"), !, add_col(3).


% punctuation(-Punctuation)//
% Punctuation marks 
punctuation('(') -->> "(",   !, inc_col.
punctuation(')') -->> ")",   !, inc_col.
punctuation(',') -->> ",",   !, inc_col.
punctuation(';') -->> ";",   !, inc_col.
punctuation(':') -->> ":",   !, inc_col.
punctuation('"') -->> """",  !, inc_col.
punctuation('nl') -->> "\n", !, inc_line.


% command(-Command)//
% Commands
command('run')     -->> lc("run"),     !, add_col(3).
command('edit')    -->> lc("edit"),    !, add_col(4).
command('list')    -->> lc("list"),    !, add_col(4).
command('llist')   -->> lc("llist"),   !, add_col(5).
command('new')     -->> lc("new"),     !, add_col(3).
command('clear')   -->> lc("clear"),   !, add_col(5).
command('cont')    -->> lc("cont"),    !, add_col(4).
command('dim')     -->> lc("dim"),     !, add_col(3).
command('let')     -->> lc("let"),     !, add_col(3).
command('def')     -->> lc("def"),     !, add_col(3).
command('cls')     -->> lc("cls"),     !, add_col(3).
command('beep')    -->> lc("beep"),    !, add_col(4).
command('rem')     -->> lc("rem"),     !, add_col(3).
command('for')     -->> lc("for"),     !, add_col(3).
command('to')      -->> lc("to"),      !, add_col(2).
command('step')    -->> lc("step"),    !, add_col(4).
command('next')    -->> lc("next"),    !, add_col(4).
command('goto')    -->> lc("goto"),    !, add_col(4).
command('on')      -->> lc("on"),      !, add_col(2).
command('gosub')   -->> lc("gosub"),   !, add_col(5).
command('return')  -->> lc("return"),  !, add_col(6).
command('if')      -->> lc("if"),      !, add_col(2).
command('then')    -->> lc("then"),    !, add_col(4).
command('print')   -->> lc("print"),   !, add_col(5).
command('lprint')  -->> lc("lprint"),  !, add_col(6).
command('input')   -->> lc("input"),   !, add_col(5).
command('data')    -->> lc("data"),    !, add_col(4).
command('read')    -->> lc("read"),    !, add_col(4).
command('restore') -->> lc("restore"), !, add_col(7).
command('stop')    -->> lc("stop"),    !, add_col(4).
command('cont')    -->> lc("cont"),    !, add_col(4).
command('end')     -->> lc("end"),     !, add_col(3).
command('locate')  -->> lc("locate"),  !, add_col(6).
% Unsupported commands in Seiko Data 2000
command('else')    -->> lc("else"),    !, add_col(4).
command('pause')   -->> lc("pause"),   !, add_col(5).


% function(-Name/Arity)//
%  Functions
function('fre'/1)     -->> lc("fre"),     !, add_col(3).
function('rnd'/1)     -->> lc("rnd"),     !, add_col(3).
function('sqr'/1)     -->> lc("sqr"),     !, add_col(3).
function('int'/1)     -->> lc("int"),     !, add_col(3).
function('abs'/1)     -->> lc("abs"),     !, add_col(3).
function('sgn'/1)     -->> lc("sgn"),     !, add_col(3).
function('cos'/1)     -->> lc("cos"),     !, add_col(3).
function('sin'/1)     -->> lc("sin"),     !, add_col(3).
function('tan'/1)     -->> lc("tan"),     !, add_col(3).
function('atn'/1)     -->> lc("atn"),     !, add_col(3).
function('exp'/1)     -->> lc("exp"),     !, add_col(3).
function('log'/1)     -->> lc("log"),     !, add_col(3).
function('inkey$'/0)  -->> lc("inkey$"),  !, add_col(6).
function('asc'/1)     -->> lc("asc"),     !, add_col(3).
function('chr$'/1)    -->> lc("chr$"),    !, add_col(4).
function('val'/1)     -->> lc("val"),     !, add_col(3).
function('str$'/1)    -->> lc("str$"),    !, add_col(4).
function('len'/1)     -->> lc("len"),     !, add_col(3).
function('left$'/2)   -->> lc("left$"),   !, add_col(5).
function('right$'/2)  -->> lc("right$"),  !, add_col(6).
function('mid$'/3)    -->> lc("mid$"),    !, add_col(4).
function('tab'/1)     -->> lc("tab"),     !, add_col(3).
function('pos'/1)     -->> lc("pos"),     !, add_col(3).
function('csrlin'/0)  -->> lc("csrlin"),  !, add_col(6).
% User-defined functions:
function(Fn/1)        -->>
  udef_fn_name(Fn),
  !,
  {atom_length(Fn, L)},
   add_col(L).


% lc(+Codes)//
% Consumed codes matching Codes are case-insensitive
lc([Code|Codes]) -->>
  [C],
  {to_lowercase_code(C, Code)},
  lc(Codes).
lc([]) -->>
  [].


% Numbers: integers (int/1), fractional (frac/2), floating point (float/3) 
% number(-NumberToken)//
% All numbers are read as positive, the minus sign (-) is a prefix 
% operator used to denote negative numbers (and change the sign)
number(NumberToken) -->>
  positive_number(NumberToken).
  
% positive_number(-Number)
positive_number(Number) -->>
  ".", % Optional integer part in a fractional number
  rest_of_non_integer_positive_number(0, Number),
  !.
positive_number(Number) -->>
  positive_integer(Integer),
  ("."
   -> rest_of_non_integer_positive_number(Integer, Number)
   ;  exponent(Exponent)
      -> {Number = float(Integer, 0, Exponent)}
      ;  {Number = int(Integer)}).
  
% rest_of_non_integer_positive_number(+Integer, -Number)//
% Return the (non-integer) number following its integer part
rest_of_non_integer_positive_number(Integer, Number) -->>
  inc_col,
  set_error(fractional),
  positive_integer(Fractional),
  set_error(fractional),
  (exponent(Exponent)
   -> {Number = float(Integer, Fractional, Exponent)}
   ;  {Number = frac(Integer, Fractional)}).

% exponent(-Exponent)//
% Return the exponent of a float (specified by "E")
exponent(Exponent) -->>
  ("e" ; "E"),
  inc_col,
  set_error(exponent),
  integer_exponent(Exponent),
  set_error(exponent).

% integer_exponent(-Exponent)//
integer_exponent(Exponent) -->>
  optional_sign(Sign, Cols),
  add_col(Cols),
  positive_integer(PosExponent),
  {Sign == '+'
   -> Exponent = PosExponent
   ;  Exponent is -PosExponent}.

% optional_sign(-Sign, -ConsumedColumns)//
% Return the sign, if present, and the number of consumed columns
optional_sign('+', 1) -->>
  "+",
  !.
optional_sign('-', 1) -->>
  "-",
  !.
optional_sign('+', 0) -->>
  [].

% positive_integer(-PositiveInteger)//
positive_integer(PositiveInteger) -->>
  digits_codes(DigitsCodes),
  {number_codes(PositiveInteger, DigitsCodes),
   length(DigitsCodes, Length)},
  add_col(Length),
  set_error(number).

% digits_codes(-DigitsCodes)//
% One or more digits
digits_codes([DigitCode|DigitsCodes]) -->>
  digit_code(DigitCode),
  !,
  more_digits_codes(DigitsCodes).
  
% more_digits_codes(-DigitsCodes)//
more_digits_codes(DigitsCodes) -->>
  digits_codes(DigitsCodes),
  !.
more_digits_codes([]) -->>
  [].
  
% digit_code(-DigitCode)//
digit_code(DigitCode) -->>
  [DigitCode],
  {"0" = [C0],
   "9" = [C9],
   DigitCode >= C0,
   DigitCode =< C9}.


% string(-String)
% Strings (str/1). Delimited by double quotes.
% Double quotes inside a string are scaped as doubling them
% Seiko Data 2000 does not seem to behave like this
string(str(String)) -->>
  """",
  rest_of_string(String).
  
% rest_of_string(-String)//
rest_of_string(String) -->>
  string_codes(StringCodes),
  """",
  !,
  {atom_codes(String, StringCodes),
   length(StringCodes, Length),
   Cols is Length+2},
  add_col(Cols).
rest_of_string(_StringCodes) -->>
  set_error(string),
  {!, fail}.

% string_codes(-Codes)//
string_codes([Code|Codes]) -->>
  """""", % Escaped double quotes
  !,
  {"""" = [Code]},
  string_codes(Codes).
string_codes([]) -->> % End of string
  {[C]=""""},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
string_codes([Code|Codes]) -->>
  [Code],
  string_codes(Codes).


% identifier(-Identifier)//
% Identifiers: either letters (numeric variables and user identifiers,
%  e.g., function definitions) or letters ending with a dollar sign 
% (for string variables)
identifier(Identifier) -->>
  letter(Code),
  alphanum_star(Codes),
  ("$"
   -> inc_col,
      {"$" = D,
       append([Code|Codes], D, DCodes), 
       atom_codes(Identifier, DCodes)}
   ;  {atom_codes(Identifier, [Code|Codes])}),
  {length([Code|Codes], Length)},
  add_col(Length).
identifier(_Identifier) -->>
  set_error(identifier),
  {!, fail}.


% alphanum_star(-Codes)//
% Zero or more alphanumeric codes
alphanum_star([Code|Codes]) -->>
  ( letter(Code)
  ; digit_code(Code)),
  alphanum_star(Codes).
alphanum_star([]) -->>
  [].

% letter(-LetterCode)//
letter(LetterCode) -->>
  [Code],
  {is_letter_code(Code),
   to_lowercase_code(Code, LetterCode)}.

% is_letter_code(-Code)
is_letter_code(Code) :-
  is_uppercase_letter_code(Code).
is_letter_code(Code) :-
  is_lowercase_letter_code(Code).

% is_uppercase_letter_code(-Code)
is_uppercase_letter_code(Code) :-
  "A" = [UA],
  "Z" = [UZ],
  UA =< Code,
  UZ >= Code,
  !.
  
% is_lowercase_letter_code(-Code)
is_lowercase_letter_code(Code) :-
  "a" = [DA],
  "z" = [DZ],
  DA =< Code,
  DZ >= Code.

% to_lowercase_code(+Code, -DCode)
to_lowercase_code(Code, DCode) :-
  is_uppercase_letter_code(Code),
  !,
  "a" = [DA],
  "A" = [UA],
  DCode is Code + DA - UA.
to_lowercase_code(Code, Code).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Position handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inc_line//
% Increment one line (resetting the column to 1)
inc_line -->>
  [add_line(1)]:position.

% inc_col//
% Increment one column
inc_col -->>
  [add_col(1)]:position.

% add_col(+N)//
% Add N columns
add_col(N) -->>
  [add_col(N)]:position.

% get_pos(-Position)
% Get the current position
get_pos(Position) -->>
  [get_pos(Position)]:position.

% acc_pos(+X, ?In, ?Out)//
% Specifying how to accumulate 'position'
acc_pos(add_col(I), pos(L, C), pos(L, C1)) :-
  C1 is C+I,
  !.
acc_pos(add_line(I), pos(L, _C), pos(L1, 1)) :-
  L1 is L+I,
  !.
acc_pos(get_pos(Position), Position, Position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set_error(+Error)
% Set the error for the longest consumed positions
set_error(Error) -->>
  get_pos(Position):position,
  {set_error('Lexical', Error, Position)}.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxilliary predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% non_visible_code(?Code)
non_visible_code(9).   % Tabulator
non_visible_code(13).  % Linefeed

% eoc//
% End of codes
eoc([], []).

% udef_fn_name(-Fn)//
% Consume the function name
udef_fn_name(Fn) -->>
  identifier(Fn),
  {udef_fn_name(Fn)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(lexer).

% Set of tests
% To test all of them: 
%   ?- lexer:test.

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.
test001 :-
  test(lexer, lex, "1", [int(1):pos(1,1)]). 

test002 :-
  test(lexer, lex, "10 1234.34 -1 -43.0", [int(10):pos(1,1),frac(1234,34):pos(1,4),op(-):pos(1,12),int(1):pos(1,13),op(-):pos(1,15),frac(43,0):pos(1,16)]). 

test003 :-
  test(lexer, lex, ".1 -.12", [frac(0,1):pos(1,1),op(-):pos(1,4),frac(0,12):pos(1,5)]). 

test004 :-
  test(lexer, lex, "1e1 1e+1 1e-1 1.1e1 1.1e+1 1.1e-1", [float(1,0,1):pos(1,1),float(1,0,1):pos(1,5),float(1,0,-1):pos(1,10),float(1,1,1):pos(1,15),float(1,1,1):pos(1,21),float(1,1,-1):pos(1,28)]). 

test005 :-
  test(lexer, lex, 'p.bas', [int(10):pos(1,1),frac(1234,34):pos(1,4),op(-):pos(1,12),int(1):pos(1,13),op(-):pos(1,15),frac(43,0):pos(1,16),punct(nl):pos(1,20),int(3):pos(2,1),punct(nl):pos(2,3),frac(1,1):pos(3,1),cmd(for):pos(3,6)]).

test006 :-
  test(lexer, lex, " """" ""ab"" ""a""""b"" ", [str(''):pos(1,2),str(ab):pos(1,5),str('a"b'):pos(1,10)]).

test007 :-
  test(lexer, lex, """ab"" 1.0", [str(ab):pos(1,1),frac(1, 0):pos(1,6)]).

test008 :-
  test(lexer, lex, "1+2*a$>=--5", [int(1):pos(1,1),op(+):pos(1,2),int(2):pos(1,3),op(*):pos(1,4),id('a$'):pos(1,5),op('>='):pos(1,7),op(-):pos(1,9),op(-):pos(1,10),int(5):pos(1,11)]).

test009 :-
  test(lexer, lex, "10 for i=1 TO n step -1", [int(10):pos(1,1),cmd(for):pos(1,4),id(i):pos(1,8),op(=):pos(1,9),int(1):pos(1,10),cmd(to):pos(1,12),id(n):pos(1,15),cmd(step):pos(1,17),op(-):pos(1,22),int(1):pos(1,23)]).

test010 :-
  test(lexer, lex, """X=""X""km""", [str('X='):pos(1,1),id(x):pos(1,5),str(km):pos(1,6)]).

test011 :-
  test(lexer, lex, """x=""x \n 1 \t2", [str('x='):pos(1,1),id(x):pos(1,5),punct(nl):pos(1,7),int(1):pos(2,2),int(2):pos(2,4)]).

test012 :-
  test(lexer, lex, "10 x=1\n20 for i=1 to 10\n30 x=x*i\n40 next i", [int(10):pos(1,1),id(x):pos(1,4),op(=):pos(1,5),int(1):pos(1,6),punct(nl):pos(1,7),int(20):pos(2,1),cmd(for):pos(2,4),id(i):pos(2,8),op(=):pos(2,9),int(1):pos(2,10),cmd(to):pos(2,12),int(10):pos(2,15),punct(nl):pos(2,17),int(30):pos(3,1),id(x):pos(3,4),op(=):pos(3,5),id(x):pos(3,6),op(*):pos(3,7),id(i):pos(3,8),punct(nl):pos(3,9),int(40):pos(4,1),cmd(next):pos(4,4),id(i):pos(4,9)]).

test013 :-
  test(lexer, lex, "1a", failure(error('Lexical', number, pos(1,2)))). 

test014 :-
  test(lexer, lex, "1.1a", failure(error('Lexical', fractional, pos(1,4)))).

test015 :-
  test(lexer, lex, "-1.a", failure(error('Lexical', fractional, pos(1,4)))).

test016 :-
  test(lexer, lex, "0.1E++2", failure(error('Lexical', exponent, pos(1,5)))).

test017 :-
  test(lexer, lex, "0.1e+2a", failure(error('Lexical', exponent, pos(1,7)))).

test018 :-
  test(lexer, lex, "10 \n 1.", failure(error('Lexical', fractional, pos(2,4)))).

test019 :-
  test(lexer, lex, "10 let sin=sin(1)", [int(10):pos(1,1),cmd(let):pos(1,4),fn(sin/1):pos(1,8),op(=):pos(1,11),fn(sin/1):pos(1,12),punct('('):pos(1,15),int(1):pos(1,16),punct(')'):pos(1,17)]).

