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
+-------------- PROGRAM ./bas/ball.bas ---------------+
|00001:01 CLS                                         |
|00010:01 DEF FNMOD10(X) = (X / 10 - INT(X / 10)) * 10|   +- SCREEN -+
|00020:01 DEF FNMOD5(X) = (X / 5 - INT(X / 5)) * 5    |   |          |
|00030:01 L0 = INT(RND(1) * 4)                        |   |          |
|00040:01 C0 = INT(RND(1) * 10)                       |   |  *       |
|00050:01 DL = INT(RND(1) * 2 - 1)                    |   |          |
|00060:01 DC = INT(RND(1) * 2 - 1)                    |   |          |
|00070:01 FOR I = 1 TO 10                             |   +lc(2,3)---+
|00080:01 REM print l;" ";c                           |
|00090:01 C1 = INT(ABS(FNMOD10(C0 + DC)))             |
+-----------------------------------------------------+
+--------- CONTROL ---------+ +--------------- INSPECT ----------------+  
| (R)UN    (S)TEP   (C)ONT  | |      C0: 2                             |  
| SET(B)K  R(E)SET  RE(N)UM | |      C1: 2                             |  
| (Q)UIT   TAB (Move Focus) | |      DC: 0                             |  
| SA(V)E  (O)PEN  REFRES(H) | |      DL: -1                            |  
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

#### Breakpoints

You can toggle a breakpoint in the first row of the panel `PROGRAM` by hitting the `b` key.
The breakpoint is displayed as an asterisk (`*`) between the statement number and the BASIC instruction.
As long as a line can contain multiple statements, each one is split in different rows in the program listing, with the format *line number*`:`*statement number*.
Thus, a breakpoint can be set in any statement of a given line, and many breakpoints can be set in a single program.

For example, given the next call:

```
debug("10 for i=0 to 9 : print i; : next i").
```

A breakpoint can be set at the second statement (`02`) of the first line (`00010`), as displayed next:

```
+-------- PROGRAM ---------+              
|00010:01 FOR I = 0 TO 9   |              
|00010:02*PRINT I;         | 
|00010:03 NEXT I           |
|                          |
+--------------------------+     
```

To set that breakpoint, first the down arrow has been pressed before setting on the breakpoint with the key `b` (then, the arrow up has been pressed to list all the program).
Note that both variables and instructions are case-insensitive.
The system can be configured to list programs in downcase (see section Configuration afterwards).

#### Configuration
File `flags.pl` contains configurations for different elements:

##### Initial locations and sizes of panels

##### Listings

* Uppercase/Downcase.
* Extra blanks to improve reading.

##### Colors

* Interface colors: Themes
* Colouring elements
   * Focused panel
   * Panel headings
   * Highlighted line
* Keywords. It should be not difficult to add syntax highlighting (`TODO`)

`TO BE CONTINUED`

