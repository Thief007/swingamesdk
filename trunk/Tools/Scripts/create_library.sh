#!/bin/sh

#
# Step 1: Detect the operating system
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

#
# Step 2: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Step 3: Setup options
#
EXTRA_OPTS=""

#
# Step 4: Usage message and process command line arguments
#
while getopts hdi o
do
    case "$o" in
    h)  Usage ;;
    d)  EXTRA_OPTS="${EXTRA_OPTS} -d";;
    i)  EXTRA_OPTS="${EXTRA_OPTS} -i";;
    [?]) print >&2 "Usage: $0 [-d] [-i] [-h]"
         exit -1;;
    esac
done

shift $((${OPTIND}-1))

#
# Step 5: Set the paths to local variables
#
SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

DIST_DIR="${SWINGAME_DIR}/Dist"
SOURCE_DIST_DIR="${DIST_DIR}/Source"


echo "--------------------------------------------------"
echo "          Creating SGSDK Library/Framework"
echo "              for $OS"
echo "--------------------------------------------------"
echo "  Extra Options: ${EXTRA_OPTS}"
echo "  Starting bundle and compile process..."
echo

#Bundle the source and build it
source ${APP_PATH}/bundle_source.sh -b ${EXTRA_OPTS} 