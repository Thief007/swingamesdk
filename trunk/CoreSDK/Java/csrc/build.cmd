@echo off

javah -classpath ..\build\classes swingame.Core swingame.Audio swingame.Input swingame.Graphics
gcc -I"%JAVA_HOME%"\include\ -I"%JAVA_HOME%"\include\win32\ -L..\build\classes -lSGSDK -shared -Wl,--add-stdcall-alias -o ..\build\classes\JavaSwinGame.dll *.c