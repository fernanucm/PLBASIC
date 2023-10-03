/*********************************************************/
/*                                                       */
/* FILE module for the BASIC language                    */
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

:- module(file,
          [ open_file_panel/2 ]).

:- use_module(flags,
          [ set_flag/1,
            file_panel_size/1,
            file_panel_location/1,
            current_open_folder/1
          ]).

:- use_module(color).

:- use_module(misc,
          [ display_frame/5,
            panel_huframe_with_title/4,
            panel_hlframe/2,
            take_n_at/4,
            blank_rows/3,
            write_at/2,
            erase_panel_background/2
          ]).


% open_file_panel(+Folder, -Outcome)
% Open a panel with the folders and files in Folder
% Return in Outcome one of:
%   open(filename)
%   cancel
%   error(message)
% Its contents are updated on each user keypress
open_file_panel(Folder, Outcome0) :-
  set_flag(current_open_folder(Folder)),
  open_file_panel(Folder, 1, 0, 0, Outcome0),
  file_panel_location(Location),
  file_panel_size(Size),
  erase_panel_background(Location, Size),
  !.

% open_file_panel(+Folder, +RowInFocus, +Offset, +Width, -Outcome)
% As open_file_panel/2 but with the current offset
% for scrolling contents
% The additional outcome 'interacting' denotes that the user
%   is still interacting with the panel
% Width is the current width of the panel, which can be enlarged but not shortened
%   (to avoid background repainting)
open_file_panel(Folder0, RowInFocus0, Offset0, Width0, Outcome0) :-
  file_listing(Folder0, RowInFocus0, Offset0, Width0-Width, Files, NbrFiles, Listing),
  display_file_panel(Listing),
  get_command(Command),
  process_panel_command(Command, Files, NbrFiles, RowInFocus0-RowInFocus, Offset0-Offset, Outcome),
  (final_outcome(Outcome)
   -> Outcome0 = Outcome
   ;  current_open_folder(Folder),
      open_file_panel(Folder, RowInFocus, Offset, Width, Outcome0)).

% display_file_panel(+Listing)
display_file_panel(Listing) :-
  file_panel_location(Location),
  file_panel_size(rc(TotalRows, Columns)),
  Width is Columns + 2,
  panel_huframe_with_title('FILES', Width, true, UFrame),
  panel_hlframe(Width, LFrame),
  display_frame(Location, TotalRows, UFrame, Listing, LFrame).


% get_command(-Command)
get_command(Command) :-
  get_single_char(Code),
  code_command(Code, Command).

% code_command(+Char, -Command)
% Return the command corresponding to the read character
code_command(27,  cancel)   :- !. % Escape key: Quit without selecting
code_command(16,  up)       :- !. % Up arrow
code_command(14,  down)     :- !. % Down arrow
code_command(13,  select)   :- !. % Intro: select file or folder to open
code_command(_,   void)     :- !. % A char not related to a command

% process_panel_command(+Command, +Files, +NbrFiles, +RowInFocus0--RowInFocus, +Offset0--Offset, -Outcome)
process_panel_command(select, Files, _NbrFiles, RowInFocus0-1, _Offset0-0, interacting) :-
  nth0(RowInFocus0, Files, folder(Folder)),
  !,
  current_open_folder(CurrentFolder),
  concat_paths([CurrentFolder, Folder], CFolder),
  normalize_path(CFolder, NewFolder),
  set_flag(current_open_folder(NewFolder)).
process_panel_command(select, Files, _NbrFiles, RowInFocus-RowInFocus, Offset-Offset, open(PathFile)) :-
  nth0(RowInFocus, Files, file(File)),
  !,
  current_open_folder(Folder),
  atomic_list_concat([Folder, '/', File], PathFile).
process_panel_command(up, _Files, _NbrFiles, RowInFocus0-RowInFocus, Offset0-Offset, interacting) :-
  !,
  process_panel_up_command(RowInFocus0-RowInFocus, Offset0-Offset, interacting).
process_panel_command(down, _Files, NbrFiles, RowInFocus0-RowInFocus, Offset0-Offset, interacting) :-
  !,
  process_panel_down_command(NbrFiles, RowInFocus0-RowInFocus, Offset0-Offset, interacting).
process_panel_command(cancel, _Files, _NbrFiles, RowInFocus-RowInFocus, Offset-Offset, cancel) :-
  !.
process_panel_command(_Command, _Files, _NbrFiles, RowInFocus-RowInFocus, Offset-Offset, interacting) :-
  !.

% process_panel_up_command(+RowInFocus0--RowInFocus, +Offset0--Offset, -Outcome)
process_panel_up_command(RowInFocus-RowInFocus, Offset-Offset, interacting) :-
  RowInFocus == 0,
  !.
process_panel_up_command(RowInFocus0-RowInFocus, Offset0-Offset, interacting) :-
  Diff is RowInFocus0 - Offset0,
  Diff >= 0,
  !,
  RowInFocus is RowInFocus0 - 1,
  L0 is RowInFocus - Offset0,
  (L0 < 0
   -> Offset is Offset0 - 1
   ;  Offset = Offset0).

% process_panel_down_command(+NbrFiles, +RowInFocus0--RowInFocus, +Offset0--Offset, -Outcome)
process_panel_down_command(NbrFiles, RowInFocus-RowInFocus, Offset-Offset, interacting) :-
  NbrFiles1 is NbrFiles - 1,
  RowInFocus == NbrFiles1,
  !.
process_panel_down_command(NbrFiles, RowInFocus0-RowInFocus, Offset0-Offset, interacting) :-
  Diff is NbrFiles - RowInFocus0 + Offset0,
  Diff > 0,
  !,
  RowInFocus is RowInFocus0 + 1,
  file_panel_size(rc(L,_)),
  L0 is RowInFocus + Offset0 + 1,
  (L0 >= L
   -> Offset is Offset0 + 1
   ;  Offset = Offset0).

% final_outcome(+Outcome)
final_outcome(open(_))  :- !.
final_outcome(cancel)   :- !.
final_outcome(error(_)) :- !.

% file_listing(+Folder, +RowInFocus, +Offset, +Width0--Width, -Files, -NbrFilesFolders, -Listing)
% Return the Files in Folder as an ordered list of folder(Name),
% and then file(Name). Also return in Listing the rows formatted
file_listing(Folder, RowInFocus, Offset, Width0-Width, Files, NbrFilesFolders, [PathRow|Listing]) :-
  folder_contents(Folder, MaxLength, Files, NbrFolders, NbrFiles),
  Width is max(Width0, MaxLength),
  NbrFilesFolders is NbrFolders + NbrFiles,
  findall(Row, 
    (nth0(Index, Files, Element),
     (Element = folder(File)
      -> format(atom(Atom), '<~w>', [File])
      ;  Element = file(Atom)),
      format_atom_to_width(Atom, Width, FRow),
     (RowInFocus == Index
      -> apply_text_color(FRow, highlighted, Row)
      ;  Row = FRow)),
    Rows),
  file_panel_size(rc(R, _)),
  set_flag(file_panel_size(rc(R, Width))),
  length(Rows, TotalRows),
  Diff is R - TotalRows,
  (Diff > 0
   -> blank_rows(Diff, Width, BlankRows),
      append(Rows, BlankRows, AllRows)
   ;  AllRows = Rows),
  current_open_folder(CurrentFolder),
  atom_length(CurrentFolder, CFLength),
  (CFLength > Width
   -> CLength is Width - 3,
      sub_atom(CurrentFolder, _Before, CLength, 0, CutCF),
      atom_concat('...', CutCF, UPathRow)
   ;  format_atom_to_width(CurrentFolder, Width, UPathRow)),
  apply_text_color(UPathRow, heading, PathRow),
  R1 is R-1,
  take_n_at(AllRows, R1, Offset, Listing).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File-related operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% folder_contents(+Folder, -MaxLength, -Files)
% Contents of Folder and tagged as either folder(FolderName)
%   or file(FileName).
% MaxLength is the maximum length of file names
folder_contents(Folder, MaxLength, Files) :-
  folder_contents(Folder, MaxLength, Files, _NbrFolders, _NbrFiles).
  
% folder_contents(+Folder, -MaxLength, -Files, -NbrFolders, -NbrFiles)
folder_contents(Folder, MaxLength, Files, NbrFolders, NbrFiles) :-
  catch(directory_files(Folder, UFiles), _, UFiles=[]),
  aggregate_all(max(Length), Length, (member(File, UFiles), atom_length(File, Length)), MaxLength),
  tag_files_folders(UFiles, Folder, TUFolders, TUFiles),
  sort(TUFolders, TFolders),
  sort(TUFiles, TFiles),
  append(TFolders, TFiles, Files), 
  length(TFolders, NbrFolders), 
  length(TFiles, NbrFiles).


% file_contents(+File, -Lines)
% Refer to server filesystem
file_contents(User, File, Lines) :-
  catch(open(File, read, In), Message, (display_message(User, Message), fail)), 
  !, 
  read_lines(In, Lines), 
  close(In).
file_contents(_User, _File, []).


% normalize_path(+UFile, -File)
% Given a path, removes occurrences of '..'
normalize_path(File0, File) :-
  working_directory(CWD, CWD), 
  relative_file_name(File0, CWD, File1),
  atom_concat('./', File1, File),
%   (File1 == ''
%    -> File = './'
%    ;  atom_concat(File1, '/', File)),
  !.


% tag_files_folders(+Entries, +Directory, -Folders, -Files)
% Tag each entry in the list Entries as either file(Entry) or folder(Entry)
tag_files_folders([], _, [], []) :-
  !.
tag_files_folders(['.'|Fs], D, Folders, Files) :-
  !,
  tag_files_folders(Fs, D, Folders, Files).
tag_files_folders([F|Fs], D, Folders, [file(F)|Files]) :-
  concat_paths([D, F], DF), 
  exists_file(DF), 
  !,
  tag_files_folders(Fs, D, Folders, Files).
tag_files_folders([F|Fs], D, [folder(F)|Folders], Files) :-
  tag_files_folders(Fs, D, Folders, Files).

% format_atom_to_width(+Atom, +Width, FormattedAtom)
format_atom_to_width(F, W, FF) :-
  format(atom(FF), '~w~` t~*+', [F, W]).


% concat_paths(+Paths, -Path).
concat_paths([A], A).
concat_paths([A, B|C], D) :-
  (atom_concat(_, '/', A) ; atom_concat('/', _, B)), 
  !, 
  atom_concat(A, B, E), 
  concat_paths([E|C], D).
concat_paths([A, B|C], D) :-
  (atom_concat(AA, '/', A) , atom_concat('/', _, B)), 
  !, 
  atom_concat(AA, B, E), 
  concat_paths([E|C], D).
concat_paths([A, B|C], D) :-
  atomic_list_concat([A, '/', B], E), 
  concat_paths([E|C], D), 
  !.

% safe_dir_file_concat(+Paths, -Path)
% Ensure a safe path: '..' should not occur to avoid file system attacks
safe_concat_paths(Paths, Path) :-
  concat_paths(Paths, Path), 
  \+((atom_concat(X, _, Path), atom_concat(_, '..', X))).


% try_close(+Stream)
% Try to close Stream. It is possible that it was already closed.
try_close(Stream) :-
  is_stream(Stream), 
  !, 
  catch(close(Stream), _, true).
try_close(_Stream).

% try_open(+File, +Mode, +Out, +Options)
% Try to open a file. If not possible, fail
try_open(File, Mode, Out, Options) :-
  catch(open(File, Mode, Out, Options), _M, fail).

% read_lines_from(+Out, +LineCount, -Lines)
% Read string lines from the stream Out, starting at (0-based) LineCount.

read_lines_from(Out, LineCount, Lines) :-
  is_stream(Out), 
  !, 
  read_lines_from(Out, 0, LineCount, Lines).
read_lines_from(_Out, _LineCount, []).

% read_lines_from(+Out, +LineNbr, +LineCount, -Lines)
read_lines_from(Out, LineNbr, LineCount, Lines) :-
  read_line_to_codes(Out, StrLine), 
  read_lines_from(Out, StrLine, LineNbr, LineCount, Lines).

read_lines_from(_Out, end_of_file, _LineNbr, _LineCount, []) :-
  !.
read_lines_from(Out, _StrLine, LineNbr, LineCount, Lines) :-
  LineNbr<LineCount, 
  !, 
  LineNbr1 is LineNbr+1, 
  read_lines_from(Out, LineNbr1, LineCount, Lines).
read_lines_from(Out, StrLine, LineNbr, LineCount, [Line|Lines]) :-
  atom_codes(Line, StrLine), 
  LineNbr1 is LineNbr+1, 
  read_lines_from(Out, LineNbr1, LineCount, Lines).


read_lines(Out, Lines) :-
  read_lines_last(Out, Lines, _LastLine).

% read_lines_last(+Out, -Lines, -LastLine)
% Read all the string lines in Lines, and the last one in LastLine from the stream Out.
read_lines_last(Out, Lines, LastLine) :-
  is_stream(Out), 
  !, 
  read_line_to_codes(Out, Line1), 
  read_lines_last(Line1, Out, Lines, end_of_file, LastLine).
read_lines_last(_Out, [], end_of_file). % Closed stream

read_lines_last(end_of_file, _, [], LastLine, LastLine) :-
  !.
read_lines_last(Codes, Out, [Line|Lines], _PreviousLine, LastLine) :-
  atom_codes(Line, Codes), 
  read_line_to_codes(Out, Line2), 
  read_lines_last(Line2, Out, Lines, Line, LastLine).

  
