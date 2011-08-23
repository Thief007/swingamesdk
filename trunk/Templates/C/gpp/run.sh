#!/bin/sh

#
# Step 1: Detect the operating system
#
MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ `uname` = "Darwin" ]; then
    OS=$MAC
elif [ `uname` = "Linux" ]; then
    OS=$LIN
else
    OS=$WIN
fi

#
# Step 2: Determine game name
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

GAME_NAME=${APP_PATH##*/}

if [ "$OS" = "$MAC" ]; then
    EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.app/Contents/MacOS/${GAME_NAME}
elif [ "$OS" = "$LIN" ]; then
    EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}
else #Windows
    EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.exe
fi

if [ ! -f "${EXE_PATH}" ]; then
    ./build.sh
fi

"$EXE_PATH"