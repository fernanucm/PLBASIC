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

`TO BE CONTINUED`

