#!/bin/bash

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

C_LIB_CODE_DIR="${SWINGAME_DIR}/Templates/C/common/lib"
CS_LIB_CODE_DIR="${SWINGAME_DIR}/Templates/CSharp/Common/lib"
OBJC_LIB_CODE_DIR="${SWINGAME_DIR}/Templates/ObjC/common/lib"

DIST_DIR="${SWINGAME_DIR}/Dist"
GENERATED_DIR="${SWINGAME_DIR}/Generated"

echo "--------------------------------------------------"
echo "          Cleaning SwinGame Distributions"
echo "--------------------------------------------------"

find ${DIST_DIR} -mindepth 1 -maxdepth 1 -type d ! -path \*.svn\* -exec rm -rf {} \;
find ${GENERATED_DIR} -type f ! -path \*.svn\* -exec rm -f {} \;

echo "  Finished"
echo "--------------------------------------------------"