#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

DIST_DIR="${SWINGAME_DIR}/Dist"
SOURCE_DIST_DIR="${DIST_DIR}/Source"

#Bundle the source and build it
${APP_PATH}/bundle_source.sh -b