rem Change the PATH environment variable for referencing SWI-Prolog 64bit
set "PATH=%PATH:C:\Program Files (x86)\SWI-Prolog=C:\Program Files\SWI-Prolog%"

rem MinGW64
set "PATH=%PATH:C:\MinGW\bin=C:\MinGW64\bin%"


gcc -c -I"C:\Program Files\SWI-Prolog 7.6.4\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -oarray.obj array.c

gcc -c -I"C:\Program Files\SWI-Prolog 7.6.4\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -oinstall.obj install.c

swipl-ld -I"C:\Program Files\SWI-Prolog 7.6.4\include" -I"%GNUWIN64%\include" -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-unused-result -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -shared -fPIC -pl swipl -o extern.dll closure.c array.c install.c
