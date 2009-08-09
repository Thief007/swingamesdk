#!/bin/bash

#
# This script contains the sh script functions related detecting the
# operating system for SwinGame
#

MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]; then
    OS=$MAC
elif [ -d /c/Windows ]; then
    OS=$WIN
else
    OS=$LIN
fi
