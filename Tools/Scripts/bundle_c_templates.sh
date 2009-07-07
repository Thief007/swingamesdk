#!/bin/sh

#
# Step 1: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Step 2: Detect the operating system (defines MAC, LIN, WIN, and OS variables)
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

C_TEMPLATE_DIR="${TEMPLATE_DIR}/C"
C_DIST_DIR="${DIST_DIR}/C"

COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"
COMMON_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/Common"

GCC_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/gcc"
GCC_C_DIST_DIR="${C_DIST_DIR}/gcc"

XCODE_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/xcode 3"
XCODE_C_DIST_DIR="${C_DIST_DIR}/xcode 3"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

#
# Step 4: Set up array of files to copy
#
COPY_LIST=( "Command line gcc,${GCC_C_TEMPLATE_DIR},${GCC_C_DIST_DIR}" )

if [ "$OS" = "$MAC" ]; then
    COPY_LIST=( "${COPY_LIST[@]}" "XCode 3,${XCODE_C_TEMPLATE_DIR},${XCODE_C_DIST_DIR}")
    
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

# Create the c code using Python
CreateCCode()
{
    cd "${PYTHON_SCRIPT_DIR}"
    python create_c_library.py
}

source ${APP_PATH}/inc/copy_without_svn.sh
source ${APP_PATH}/inc/dist_dir.sh

#
# Step 6: Create c library and copy
#
echo "--------------------------------------------------"
echo "          Creating SwinGame C Templates"
echo "              for $OS"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "
ListDists "${COPY_LIST}"
echo "--------------------------------------------------"
echo "  Python scripts at $PYTHON_SCRIPT_DIR"

if [ "$OS" = "$MAC" ]; then
    echo "  Copying Frameworks from Source dist"
elif [ "$OS" = "$WIN" ]; then
    echo "  Copying libraries from Source dist"
fi

echo "--------------------------------------------------"
echo "  ... Creating C library code"
CreateCCode

DoDist "${COPY_LIST}" "${C_DIST_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_C_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"
