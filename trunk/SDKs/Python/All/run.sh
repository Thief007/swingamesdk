#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 

PYTHON_PATH="/System/Library/Frameworks/Python.framework/Versions/2.5/Resources/Python.app/Contents/MacOS/Python"
PY_APP=`which python`

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    if [ -d ./bin ]
    then
        rm -rf ./bin
    fi
    mkdir ./bin
    
    cd ./bin
    ln -s $PYTHON_PATH MyPy
    ln -s ../sgsdk sgsdk
    PY_APP=./MyPy
    $PY_APP ../$1
    cd ..
else
    $PY_APP $1
fi