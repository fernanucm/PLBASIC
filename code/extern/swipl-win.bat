set cwd %cd%
:begin
cls
call "C:\Program Files\SWI-Prolog 8.5.11-1\bin\swipl-win.exe" -g "set_prolog_flag(double_quotes, codes)" -g "load_foreign_library(extern)"
pause
cd %cwd%
goto begin
