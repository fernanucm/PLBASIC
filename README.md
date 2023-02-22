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

### Caveats
* Contents will be uploaded as time permits.
* `TODO` indicates tasks to be done (in the expected near future).

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

From the SWI-Prolog program prompt, write the following to consult and start the debugger for a given program (`reverse.bas` as an example):

```
?- [debug].
?- debug('./bas/reverse.bas').
```

This leads to the debugger user interface, which consists of several panels:

* `PROGRAM`. The loaded program, if any.
* `SCREEN`. The same as in the interpreter.
* `CONTROL`. Available commands, executed by pressing the letter between brackets or the special keys.
* `INSPECT`. List of variables found along executing the program, displaying their values.
* `FILES`. A basic file explorer to open BASIC programs.

```
+-------- PROGRAM ./bas/ball.bas --------+             
|00001:01 CLS                            |             
|00010:01 DEF FNMOD10(X) = (X / 10 - INT(|         +- SCREEN -+
|         X / 10)) * 10                  |         |          |
|00020:01 DEF FNMOD5(X) = (X / 5 - INT(X |         |          |
|         / 5)) * 5                      |         |      *   |
|00030:01 L0 = INT(RND(1) * 4)           |         |          |
|00040:01 C0 = INT(RND(1) * 10)          |         |          |
|00050:01 DL = INT(RND(1) * 2 - 1)       |         +lc(2,7)---+
|00060:01 DC = INT(RND(1) * 2 - 1)       |
|00070:01 FOR I = 1 TO 10                |
+----------------------------------------+
+--------- CONTROL ---------+ +--------------- INSPECT ----------------+  
| (R)UN    (S)TEP   (C)ONT  | |      C0: 6                             |  
| SET(B)K  R(E)SET  RE(N)UM | |      C1: 6                             |  
| (Q)UIT   TAB (Move Focus) | |      DC: 0                             |  
| SA(V)E  (O)PEN  REFRES(H) | |      DL: 0                             |  
|     (M)OVE   RESI(Z)E     | |       I: 11                            |  
|       Arrow Up/Down       | |      L0: 2                             |  
| Page (U)p/(D)own: Paging  | |      L1: 2                             |  
| Home/End: First/last page | |                                        |  
|---------------------------| |                                        |  
|Status : Program ended     | |                                        |  
+---------------------------+ +----------------------------------------+ 
```

The arrangement of panels may differ from this example, which corresponds to the execution (command `RUN` executed by pressing `r`) of the example program `ball.bas` (bouncing ball). They can be moved and resized at will with the the arrow keys when the command `MOVE` or `RESIZE` have been enabled by pressing the keys `m` and `z` respectively.

#### Commands

The `CONTROL` panel lists the available commands and their keyboard shorcuts between parentheses. 
These commands are the following:

* `RUN`:       Execute the program up to its end or first breakpoint
* `STEP`:      Execute the current statement (which is shown highlighted)
* `% STOP`: Stop the execution (`TODO`. Requires external DLL to be built)
* `CONT`:      Continue the execution
* `SETBK`:     Set breakpoint in the current (highlighted) line
* `RESET`:     Reset the debugging session (reload the program)
* `RENUM`:     Renum the program and reset
* `OPEN`:      Open a file from a selectable folder
* `SAVE`:      Save the loaded program (useful after a renumbering)
* `QUIT`:      Quit the debugger
* `REFRESH`:   Clear the host screen and redraw
* `TAB`:       Move focus to the next panel
* `RESIZE`:    Let the panel in focus to be resized with the arrow keys
* `MOVE`:      Let the panel in focus to be moved with the arrow keys
* `ArrowUp`:   Scroll up/Vertical Downsize/Move Up the panel in focus
* `ArrowDown`: Scroll down/Vertical Upsize/Move Down the panel in focus
* `ArrowLeft`: Horizontal Downsize/Move Left the panel in focus
* `ArrowRight`: Horizontal Upsize/Move Right the panel in focus
* `PageUp`:    Page up the panel in focus
* `PageDown`:  Page down the panel in focus
* `Home`:      Go to the first line of the panel in focus
* `End`:       Go to the last line of the panel in focus



`TO BE CONTINUED`

