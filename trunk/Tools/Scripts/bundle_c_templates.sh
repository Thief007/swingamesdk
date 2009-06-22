#!/bin/sh

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    OS="Mac"
else
    OS="Linux"
fi

echo "--------------------------------------------------"
echo "          Creating SwinGame C Templates"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "
echo "    - Command Line gcc"
echo "    - Eclipse CDT"

if [ $OS = "Mac" ]
then
    echo "    - XCode 3"
fi