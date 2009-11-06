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

echo "--------------------------------------------------"
echo "          Cleaning SwinGame Distributions"
echo "--------------------------------------------------"

find ${DIST_DIR} -mindepth 1 -maxdepth 1 -type d ! -path \*.svn\* -exec rm -rf {} \;

rm -f ${C_LIB_CODE_DIR}/*.c
rm -f ${C_LIB_CODE_DIR}/*.h
svn up ${C_LIB_CODE_DIR}
if [ $? != 0 ]; then exit 1; fi

rm -f ${CS_LIB_CODE_DIR}/*.cs
svn up ${CS_LIB_CODE_DIR}
if [ $? != 0 ]; then exit 1; fi

rm -f ${OBJC_LIB_CODE_DIR}/*.c
rm -f ${OBJC_LIB_CODE_DIR}/*.m
rm -f ${OBJC_LIB_CODE_DIR}/*.h
svn up ${OBJC_LIB_CODE_DIR}
if [ $? != 0 ]; then exit 1; fi

echo "  Finished"
echo "--------------------------------------------------"