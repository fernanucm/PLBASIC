rem Change the PATH environment variable for referencing SWI-Prolog 64bit
set "PATH=%PATH:C:\Program Files (x86)\SWI-Prolog=C:\Program Files\SWI-Prolog%"

rem Change the PATH environment variable for referencing SWI-Prolog 8.5.11-1 64bit
set "PATH=%PATH:C:\Program Files\SWI-Prolog 7.6.4=C:\Program Files\SWI-Prolog 8.5.11-1%"

rem MinGW64
set "PATH=%PATH:C:\MinGW\bin=C:\MinGW64\bin%"

set SWIPL=C:\Program Files\SWI-Prolog 8.5.11-1


gcc -c -I"%SWIPL%\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -opkbhit.obj pkbhit.c

gcc -c -I"%SWIPL%\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -oarray.obj array.c

gcc -c -I"%SWIPL%\bin" -I"%SWIPL%\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -oinstall.obj install.c

swipl-ld -I"%SWIPL%\bin" -I"%SWIPL%\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -shared -fPIC -pl swipl -o extern.dll closure.c array.c pkbhit.c install.c
