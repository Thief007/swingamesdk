#!/bin/sh

fpc -g -k"-arch i386" -S2 PascalSDLTypeChecker.pas -k"-F ../Frameworks -framework SDL"

gcc -g -arch i386 -Wl,-rpath,@loader_path/../Frameworks -F ../Frameworks -framework SDL -L. -l PascalSDLTypeChecker sample_type_check.c

./a.out