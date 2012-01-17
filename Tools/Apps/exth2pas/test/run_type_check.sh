#!/bin/sh

fpc -k"-arch i386" -S2 PascalSDLTypeChecker.pas -k"-F ../Frameworks -framework SDL"

gcc -arch i386 -Wl,-rpath,@loader_path/../Frameworks -F ../Frameworks -framework SDL -L. -l PascalSDLTypeChecker sample_type_check.c
