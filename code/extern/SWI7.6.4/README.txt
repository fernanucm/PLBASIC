1- Generate library 'extern':

make.bat

2- Run SWI-Prolo 7.6.4:

Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

1 ?- load_foreign_library(extern).
true.

2 ?- foreign:ext_closure([sim(a, b, 0.8), sim(b, c, 0.3)], 7, 1, sim, 0, _21522).
_21522 = [sim(c, b, 0.3), sim(c, a, 0.3), sim(b, c, 0.3), sim(b, a, 0.8), sim(a, c, 0.3), sim(a, b,
 0.8)].

3 ?- halt.