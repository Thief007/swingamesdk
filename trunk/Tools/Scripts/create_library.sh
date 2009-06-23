#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

DIST_DIR="${SWINGAME_DIR}/Dist"
SOURCE_DIST_DIR="${DIST_DIR}/Source"

EXTRA_OPTS=""

while getopts hdi: o
do
    case "$o" in
    h)  Usage ;;
    d)  EXTRA_OPTS="${EXTRA_OPTS} -d";;
    i)  EXTRA_OPTS="${EXTRA_OPTS} -i $OPTARG";;
    [?]) print >&2 "Usage: $0 [-d] [-i local|global] [-h]"
         exit -1;;
    esac
done

shift $((${OPTIND}-1))

echo "--------------------------------------------------"
echo "          Creating SGSDK Library/Framework"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Extra Options: ${EXTRA_OPTS}"
echo "  Starting bundle and compile process..."
echo

#Bundle the source and build it
source ${APP_PATH}/bundle_source.sh -b ${EXTRA_OPTS} 