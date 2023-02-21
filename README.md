# PLBASIC
Interpreter and debugger of BASIC programs implemented in Prolog

## Credits
The interpreter follows the (great) implementation and some guidelines of [`victorlagerkvist`](https://prologomenon.wordpress.com/2020/10/25/writing-a-basic-interpreter-part-1/).
Any bug and bad design decisions in PLBASIC must be blamed on me.

## Introduction
This repository includes a text-based interface for both an interpreter and debugger of BASIC programs, implemented in SWI-Prolog.
Being multiplatform, it can be used directly from sources (folder `./src`) having installed SWI-Prolog 9.0 or above. 
In addition, an installer is provided for Windows, should you do not want to install SWI-Prolog.
The installer (`setup.exe`) has been tested in Windows 10 64 bit (folder `./W10Installer`).
This project was motivated to emulate the BASIC implementation in the Seiko Data 2000 watch from the 80's, coupled with the Seiko UC 2200 for writing (and printing) programs.
Thus, the screen is originally set to a small size of 5 lines and 10 columns, but can be enlarged to other sizes (see the `flags.pl` file in the `./src` folder).
The intention of the project is to be neither complete nor accurate, but a raw approximation to the real thing.
Some features are not present while others are added.

### Caveat
Contents will be uploaded as time permits.

## A First Glance

### The BASIC Interpreter started from SWI-Prolog

After starting SWI-Prolog from the `./src` folder with either pl in Linuxes or swipl-win.exe in Windows, write the following at the SWI-Prolog prompt:

```
?- [basic].
?- run("10 print ""Hello!""").
```

This results in:

```
+- SCREEN -+
|Hello!    |
|          |
|          |
|          |
|          |
+lc(1,0)---+
```

Here, SCREEN is the panel surrounding the watch screen. Below, the numbers show the current line an column (both 0-based) of the cursor.

Instead of passing the program as shown, a file can be selected otherwise, such as in:

```
?- run('./bas/factorial.bas').
```

(notice the single quotes `'` delimiting the file path as opposed to double quotes `"`) which displays:

```
+- SCREEN -+
| 120      |
|          |
|          |
|          |
|          |
+lc(1,0)---+
```

i.e., the result of computing the factorial of 5.

### The BASIC Debugger started from SWI-Prolog

While the interpreter is a simple way to executing BASIC progrmas, the debugger (not present in the original Seiko watch) includes expected features of a simple debugger: step-by-step, breakpoints, inspects and the like.

`TO BE CONTINUED`

