% Expression parser
% Adapted from: "The simple and powerful yfx operator precedence parser", E. L. Favero,2007, Softw. Pract. Exper. 37:1451–1474, Wiley

expr(E) -->
  % Start with the lowest precedence (1 is the maximum precedence)
  expr(E, 1200).
  
% Op: Operator
% LP: Left Precedence, RP: Right Precedence, PP: Principal Precedence
expr(E, PP) --> % Factor
  [int(I)],
  !,
  rExpr(int(I), E, 0, PP).
expr(E, PP) --> % Factor
  [frac(I, F)],
  !,
  rExpr(frac(I, F), E, 0, PP).
expr(E, PP) --> % Factor
  [float(I, F, E)],
  !,
  rExpr(float(I, F, E), E, 0, PP).
expr(E, PP) --> % Factor
  [id(Id)],
  !,
  rExpr(id(Id), E, 0, PP).
expr(E, PP) --> % Factor
  [strid(SId)],
  !,
  rExpr(strid(SId), E, 0, PP).
expr(E, PP) --> % Factor
  [punct('(')],
  expr(TI),
  [punct(')')],
  !,
  rExpr(TI, E, 0, PP).
expr(E, PP) -->
  [op(Op)],
  {prefix(Op, P, RP),
   P =< PP,
   (redef(Op) -> true ; !)},
  expr(Arg, RP),
  {NE =.. [Op, Arg]},
  rExpr(NE, E, P, PP).

% rExpr(+Expr, -E, +LeftP, -PP)
rExpr(Expr, E, LeftP, PP) -->
  [op(Op)],
  {infix(Op, P, LP, RP),
   P =< PP, 
   LeftP =< LP,
   (redef(Op) -> true ; !)},
  expr(Arg2, RP),
  {NE =.. [Op, Expr, Arg2]},
  rExpr(NE, E, P, PP).
rExpr(Expr, E, LeftP, PP) -->
  [op(Op)],
  {posfix(Op, P, LP),
   P =< PP,
   LeftP =< LP,
   (redef(Op) -> true ; !),
   NE =.. [Op, Expr]},
  rExpr(NE, E, P, PP).
rExpr(E, E, _, _) -->
  [].


% Operators
% prefix(P, Op, P-1) :- op(P, fx, Op).
% prefix(P, Op, P)   :- op(P, fy, Op).
prefix(+, 380, 380).
prefix(-, 380, 380).

% infix(P, Op, P-1, P-1)  :- op(P, xfx, Op).
% infix(P, Op, P-1, P)    :- op(P, xfy, Op).
infix(^, 200, 199, 200).

% infix(P, Op, P,   P-1)  :- op(P, yfx, Op).
infix(+, 500, 500, 499).
infix(-, 500, 500, 499).
infix(*, 400, 400, 399).
infix(/, 400, 400, 399).

% posfix(P, Op, P-1) :- op(P, xf, Op).
% posfix(P, Op, P)   :- op(P, yf, Op).
posfix(!, 280, 280).

% redef(Op)
% Redefined operators
redef(+).
redef(-).

% ?-  phrase(expr(E), [int(4),op(+),int(1),op(^),int(2),op(^),int(2)]), write_canonical(E).