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

2 ?- repeat, kbhit(Code), writeln(Code), sleep(0.2), Code == 32, !.

load_foreign_library(extern).

repeat, kbhit(Code), writeln(Code), sleep(0.2), Code == 32, !.

repeat, interpreter:kbhit(Code), sleep(0.1), Code == 32, !.
