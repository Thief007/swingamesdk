#!/bin/sh

javah -classpath ../build/classes swingame.Core swingame.Audio swingame.Input swingame.Graphics

gcc -dynamiclib ../../SGSDK.NET/lib/*.o -I$JAVA_HOME/Include `cat ../../src/maclinki386.res` *.c -o ../build/classes/libJavaSwinGame.jnilib -framework JavaVM -framework Cocoa -framework Foundation