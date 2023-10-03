rem Change the PATH environment variable for referencing SWI-Prolog 64bit
set "PATH=%PATH:C:\Program Files (x86)\SWI-Prolog=C:\Program Files\SWI-Prolog%"

rem Change the PATH environment variable for referencing SWI-Prolog 8.5.11-1 64bit
set "PATH=%PATH:C:\Program Files\SWI-Prolog 7.6.4=C:\Program Files\SWI-Prolog 8.5.11-1%"

rem MinGW64
set "PATH=C:\Program Files\SWI-Prolog 8.5.11-1\bin;C:\MinGW64\bin"

set SWIPL=C:\Program Files\SWI-Prolog 8.5.11-1

set INCLUDE=
set LIB=
set MYPATH=
set VS120COMNTOOLS=
set VS140COMNTOOLS=


swipl-ld -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -shared -fPIC -pl swipl -o extern.dll kbhit.c install.c
