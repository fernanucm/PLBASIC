# PLBASIC
Interpreter and debugger of BASIC programs implemented in Prolog


## Credits
The interpreter follows the (great) implementation and some guidelines of [`victorlagerkvist`](https://prologomenon.wordpress.com/2020/10/25/writing-a-basic-interpreter-part-1/).
Any bug and bad design decisions in PLBASIC must be blamed on me.


## 1. Introduction
This repository includes a text-based interface for both an interpreter and debugger of BASIC programs, implemented in SWI-Prolog.
Being multiplatform, it can be used directly from sources (folder `./src`) having installed SWI-Prolog 9.0 or above. 
In addition, an installer is provided for Windows, should you do not want to install SWI-Prolog.
The installer (`setup.exe`) has been tested in Windows 10 64 bit (folder `./W10Installer`).

This project was motivated to emulate the Microsoft BASIC implementation in the Seiko UC 2000 watch from the 80's, coupled with the Seiko UC 2200 for writing (and printing) programs.


<img src="http://i.imgur.com/iAxehPR.jpg" alt= "SEIKO UC 2000 watch and UC 2200 keyboard and printer" width="300px">

Image source: [TECHEBLOG](https://www.techeblog.com/before-smartwatches-there-was-the-seiko-uc-2000-wrist-computer-here-are-5-cool-facts/)

Thus, the screen is originally set to a small size of 5 lines and 10 columns, but can be enlarged (or shortened!) to other sizes (see the `flags.pl` file in the `./src` folder).

The intention of the project is to be neither complete nor accurate, but a raw approximation to the real thing.
Some features are not present while others are added.


### 1.1. Caveats
* Contents will be uploaded as time permits.
* `TODO` indicates tasks to be done (in the expected near future).


## 2. A First Glance

### 2.1. The BASIC Interpreter started from SWI-Prolog

Start SWI-Prolog from the `./src` folder with either `pl` in Linuxes or `swipl-win.exe` in Windows, setting the behavior of double_quotes to codes.
For example, open a `cmd` terminal in Windows and type:

```bat
C:\PLBASIC> swipl-win.exe -g "set_prolog_flag(double_quotes, codes)"
```

Then, write the following at the SWI-Prolog prompt:

```prolog
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

```prolog
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


### 2.2. The BASIC Debugger started from SWI-Prolog

While the interpreter is a simple way to executing BASIC programs, the debugger (not present in the original Seiko watch) includes expected features of a simple debugger: step-by-step, breakpoints, inspects and the like.

From the SWI-Prolog program prompt, write the following to consult and start the debugger for a given program (`reverse.bas` as an example):

```prolog
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

#### Debugger Commands

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

##### Implementation note: 

Since the `PageUp` and `PageDown` keys do not return an ASCII code that can be read with `get_single_char/1`, other keys have been selected as shortcuts (`u` and `d`, respectively).
As well, the `STOP` command requires reading the keyboard without waiting for a key press, which can be better implemented with a new foreign predicate written in C/C++ (expected to be completed in the near future).

#### Commands of the `FILES` panel

This panel is opened with the debugger command `OPEN`, leading to something like this:

```
+--- FILES ---+
|./bas        |
|<..>         |
|ball.bas     |
|factorial.bas|
|lander.bas   |
|matches.bas  |
|reverse.bas  |
|             |
+-------------+
```

The first row in the panel is the current folder.
The highlighted row is the selected row for either opening the file or navigating into the folder.
Folders are enclosed between angle brackets, and `<..>` means to go up in the folder tree. 

The following commands are available for this panel:

* `ArrowUp`:   Scroll up the panel
* `ArrowDown`: Scroll down the panel
* `ESCAPE`:    Cancel the `OPEN` command
* `INTRO`:     Select the file to open / Navigate to the folder


#### Breakpoints

You can toggle a breakpoint in the first row of the panel `PROGRAM` by hitting the `b` key.
The breakpoint is displayed as an asterisk (`*`) between the statement number and the BASIC instruction.
As long as a line can contain multiple statements, each one is split in different rows in the program listing, with the format *line number*`:`*statement number*.
Thus, a breakpoint can be set in any statement of a given line, and many breakpoints can be set in a single program.

For example, given the next call:

```prolog
?- debug("10 for i=0 to 9 : print i; : next i").
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
The system can be configured to list programs in downcase letters (see next section Configuration).


#### Configuration
File `flags.pl` contains configurations for different elements:


##### Initial locations and sizes of panels

Screen:
```prolog
screen_panel_location(lc(0, 43)).
screen_panel_size(rc(5, 10)).
```

Program:

```prolog
program_panel_location(lc(0, 0)).
program_panel_size(rc(10, 40)).
```

Inspect:
```prolog
inspect_panel_location(lc(25, 0)).
inspect_panel_size(rc(10, 40)).
```

Control:
```prolog
control_panel_location(lc(13, 0)).
control_panel_size(rc(10, 27)).
```


##### Listings

* Uppercase/Downcase.
```prolog
uppercase(on). % Default is uppercase (change to off otherwise)
```

* Extra blanks to improve reading.

```prolog
optional_spaces(on). % Change to off to remove optional spaces
```

* `LET` instruction.
```prolog
let(off). % Change to on if you prefer LET to appear
```


##### Colors

* Interface colors: Themes

  SWI-Prolog allows to load color themes as defined in Prolog files.
  This project includes a couple of themes in the folder `./src/themes`.
  For example, to select white letters over a blue background (as some old 8 bit computers enjoyed):

  ```prolog
  ?- [library('./themes/blue_while.pl')].
  ```

  You can change or add new themes at will in these themes.

* Coloring elements

  File `color.pl` contains color definitions for different elements (focused panels, panel headings and highlighted lines).
  Available color names are listed in the predicate `color/3`, and resumed next:
  
  `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, `bright_black`, `bright_red`, `bright_green`, `bright_yellow`, `bright_blue`, `bright_magenta`, `bright_cyan`, `bright_white`
  
    * Focused panel (title bar)
   
    ```prolog
    text_color(focused, fb(bright_blue, white)).
    ```

    * Panel headings (such as the path in the `FILE` panel)
   
    ```prolog
    text_color(heading, fb(bright_white, bright_black)).
    ```
    
    * Highlighted line (in the `PROGRAM` panel)
   
    ```prolog
    text_color(highlighted, fb(bright_blue, white)).
    ```
    
* Keywords. It should not be difficult to add syntax highlighting (`TODO`)

## Supported Instruction Set

Following the (acknowledeged incomplete) [online manual](https://gtello.pagesperso-orange.fr/seikomanual.pdf) of Guillaume Tello (2008) for this watch, the next instructions, functions and operators below are supported:

### Instructions

* `RUN [li]`
  Runs the program [starting at line li].
  
* `LIST [li]`
  Display the listing of the program using the line number order.	With li specified, starts from this line til the end.

* `NEW`
  Erases the whole program and variables from memory.
  
* `CLEAR`
  Erases all variables values and definitions (those declared with DIM). The program remains in memory.
  
* `CONT`
  If a program is stopped with STOP (instruction or key), you can go on with it using this command. Program should not be modified, variables can.

* `DIM var(n1 [,n2,..,nmax])`
	Declares var as an array, it seems that max=5.
  
* `[LET] var=expression`
  Computes the expression and stores it into var. LET is optional.
  
* `DEF FNvar(x)=..expression of x..`
  Declares FNvar as a function, then to use it, as a numeric value, just write FNvar(x).

* `CLS`
  Clears the screen and the cursor goes to the upper left.
  
* `BEEP`
  Outputs a beep sound.

* `REM text`
  Inserts a remark in the program. 
  
* `FOR var=x TO x’ [STEP x’’]`
	`instructions`
  `NEXT var`
  Initialises `var` to `x` and repeats the instruction block until `var>x’`.
	If no `STEP` is specified then, `STEP=1` to increment var at each loop.
	If `x’’<0`, then loop until `var<x’`.

* `GOTO li`
  Jumps to the specified line.

* `ON n GOTO li1 [, li2…., limax]`
  Computes the integer value of `n`, and, if `n=1` jumps to `li1`, if `n=2` to li2, etc.
	if `n<1` or `n>max` then no jump is performed and go to the next instruction.

* `IF condition THEN [instructions|li]`
  If a condition is true (different from zero) then the following instructions are executed, or jump to the specified line number li. If the condition is false (equal to zero) then executions goes on to the next line.

* `PRINT [ str or x[, or ; [etc…]] ]`
  If separated by `;` they are displayed next to the previous `;` if separated by `,` an 8-width tabulator is applied between values. Each numeric value is preceeded by a space if positive or by a `-` if negative.
  
* `INPUT [«message» ;]var1[,var2,… ]`
  Display the message, if present, and ask for the values of each variable. You must separate the values with `,`. 
  
* `DATA value_list`
  Inserts a list of immediate values (reals or strings, but not expressions) in the program. 

* `READ var1 [,var2,…]`
  Allows you to parse them (as an `INPUT` but without a human intervention).

* `RESTORE [li]`
  Tells from which DATA line the next `READ` should pick its values.
  
* `STOP`
  Stops the execution.
  
* `CONT`
  Resumes a stopped execution.
  
* `END`
  Stops the program. Cannot be resumed.

### Functions

* `FRE(x)`
	Returns the number of free bytes, 2922 at start. The argument `x` is ignored.

* `RND(x)`
	Returns a pseudo-random number from in [0;1[.
	* If `x>0`, returns the next random number.
	* If `x=0`, returns the last random number.
	* If `x<0`, initilalizes a new serie of random numbers according to `x`.

* `SQR(x)`
	Returns the square root of the positive value `x`.

* `INT(x)`
	Returns the highest integer lower or equal to `x`.

* `ABS(x)`
	Returns the absolute value of `x`.

* `SGN(x)`
	Returns the sign of `x` in this way:
	* if `x>0` then `SGN(x)=1`
	* if `x<0` then `SGN(x)=-1`
	* if `x=0` then `SGN(x)=0`

* `COS(x)`
Returns the cosine of the `x` argument.	The angle is in radians.

* `SIN(x)`
Returns sine of the `x` argument.	The angle is in radians.

* `TAN(x)`
Returns the tangent of the `x` argument. The angle is in radians.

* `ATN(x)`
	Returns the reverse tangent of the `x` argument. The angle is in radians.

* `EXP(x)`
	Returns the exponential of `x` (base is e=2.71828…).

* `LOG(x)`
	Returns the logarithm of `x` (base is e=2.71828…).

* `INKEY$` `TODO` (Requires the external predicate to be built)
	Returns the current key pressed, else a null string.

* `ASC(str)`
  Returns the ASCII code of the first character of the string.
  
* `CHR$(x)`
	Returns a one character string corresponding to the `x` ASCII code.

* `VAL(str)`
  Returns a real described in the string.

* `STR$(x)`
	Returns the string descibing the real `x`.

* `LEN(str)`
	Returns the length of `str`.

* `LEFT$(str,n)`
	Returns a string with the `n` leftmost characters of `str`.

* `RIGHT$(str,n)`
	Returns a string with the n rightmost characters of str.

* `MID$(str,n1,n2)`
	Returns a string with `n2` characters long starting at position `n1` in str.

* `TAB(n)`
  Specifies how many spaces before next printing. It is used as part of `PRINT`.
  
* `POS(0)`
	Pseudo-variable that returns the current cursor column from 0 to 9.

* `CSRLIN`
	Pseudo variable that returns the current cursor line from 0 to 3.

* `LOCATE n1,n2`
	Places the cursor at the given location:
	`n1` is the column from 0 to 9
	`n2` is the line from 0 to 3.

### Operators

* Arithmetic operators: `+` `-` `*` `/` `^`

* Logical operators: `=` `>` `<` `>=` `>=` `<>` `AND` `OR` `NOT`

Parentheses can be used as needed to surround expressions.

### Additional supported features

* `IF condition THEN [instructions|li] ELSE [instructions|li]`

* Fractional numbers.

* Values in `READ` can be expressions.


### Unsupported features

* Interactive commands at the screen.

* In the original system, the `;` separator could be omitted between an immediate string and a value. For example: `PRINT "X=";X;"Km" can be compacted as `PRINT “X=”X”Km”`. In this implementation, this is not allowed.

* `EDIT [li]`
  Edition of line `li`.
  
* `LLIST [li]`
  Same as `LIST` but printed on paper instead of displayed on screen.

* `LPRINT [str or x[, or ; [etc…]]]`
  The same as `PRINT` but for the printer

* Error messages. (`TODO`)


`TO BE CONTINUED`

