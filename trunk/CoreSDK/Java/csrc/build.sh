#!/bin/sh

javah -classpath ../bin swingame.platform.NativeCore swingame.platform.NativeInput swingame.platform.NativeGraphics

gcc -dynamiclib ../../SGSDK.NET/lib/*.o -I$JAVA_HOME/Include `cat ../../src/maclinki386.res` *.c -o ../clib/libJavaSwinGame.jnilib -framework JavaVM -framework Cocoa -framework Foundation