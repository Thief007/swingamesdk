@echo off

javah -classpath ..\bin swingame.Core swingame.Audio swingame.Input swingame.Graphics
gcc -I"%JAVA_HOME%"\include\ -I"%JAVA_HOME%"\include\win32\ -L..\lib -lSGSDK -shared -Wl,--add-stdcall-alias -o ..\clib\JavaSwinGame.dll *.c