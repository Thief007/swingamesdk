#!/bin/sh

#
# Step 1: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Step 2: Detect the operating system
#
source "${APP_PATH}/inc/os_check.sh"

#
# Step 3: Set the paths to local variables
#
SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

PYTHON_SCRIPT_DIR=${SWINGAME_DIR}/Tools/SGWrapperGen

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
DIST_DIR="${SWINGAME_DIR}/Dist"

PAS_TEMPLATE_DIR="${TEMPLATE_DIR}/Pascal"
PAS_DIST_DIR="${DIST_DIR}/Pascal"

COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"
COMMON_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/Common"

FPC_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/FPC"
FPC_PAS_DIST_DIR="${PAS_DIST_DIR}/FPC"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

#
# Step 4: Set up array of files to copy
#
COPY_LIST=( "Command line FPC,${FPC_PAS_TEMPLATE_DIR},${FPC_PAS_DIST_DIR}" )

if [ "$OS" = "$MAC" ]; then
    # build framework if needed
    if [ ! -d "${SOURCE_DIST_DIR}/bin/SGSDK.framework" ]; then
        source ${APP_PATH}/bundle_source.sh -b
        if [ $? != 0 ]; then echo "Error building SGSDK framework"; exit 1; fi
        echo
        echo
    fi
elif [ "$OS" = "$WIN" ]; then
    # build dll if needed
    if [ ! -f "${SOURCE_DIST_DIR}/bin/SGSDK.dll" ]; then
        source ${APP_PATH}/bundle_source.sh -b
        if [ $? != 0 ]; then echo "Error building SGSDK library"; exit 1; fi
        echo
        echo
    fi
fi

#
# Step 5: Declare functions
#
source ${APP_PATH}/inc/copy_without_svn.sh
source ${APP_PATH}/inc/dist_dir.sh

#
# Step 6: Copy Pascal Library
#
echo "--------------------------------------------------"
echo "      Creating $OS SwinGame Pascal Templates"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "
ListDists "${COPY_LIST}"
echo "--------------------------------------------------"

DoDist "${COPY_LIST}" "${PAS_DIST_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_PAS_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"
