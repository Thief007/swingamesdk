#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

C_LIB_CODE_DIR="${SWINGAME_DIR}/Templates/C/common/lib"

DIST_DIR="${SWINGAME_DIR}/Dist"

echo "--------------------------------------------------"
echo "          Cleaning SwinGame Distributions"
echo "--------------------------------------------------"

find ${DIST_DIR} -type d ! -path \*.svn\* -mindepth 1 -maxdepth 1 -exec rm -rf {} \;

rm -f ${C_LIB_CODE_DIR}/*.c
rm -f ${C_LIB_CODE_DIR}/*.h

echo "  Finished"
echo "--------------------------------------------------"