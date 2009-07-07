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
# Step 3: Setup options
#
BUILD="N"
EXTRA_OPTS=""

while getopts bid o
do
    case "$o" in
    b)  BUILD="Y";;
    d)  BUILD="Y"
        EXTRA_OPTS="${EXTRA_OPTS} -d";;
    i)  EXTRA_OPTS="${EXTRA_OPTS} -i" ;;
    [?]) print >&2 "Usage: $0 [-b] [-d] [-i] [version]"
         exit -1;;
    esac
done

shift $((${OPTIND}-1))

if [ "a$1" != "a" ]; then
    EXTRA_OPTS="${EXTRA_OPTS} $1"
fi

#
# Step 5: Set the paths to local variables
#
SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

SDK_SRC="${SWINGAME_DIR}/CoreSDK/src"
SDK_LIB_SRC="${SWINGAME_DIR}/CoreSDK/libsrc"

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
DIST_DIR="${SWINGAME_DIR}/Dist"

SOURCE_TEMPLATE_DIR="${TEMPLATE_DIR}/Source"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

SRC_DIR="${SOURCE_DIST_DIR}/src"

C_TEMPLATE_DIR="${TEMPLATE_DIR}/C"
C_COMMON_LIB_DIR="${C_TEMPLATE_DIR}/common/lib"

PYTHON_SCRIPT_DIR=${SWINGAME_DIR}/Tools/SGWrapperGen

if [ "$OS" = "$MAC" ]; then
    FRAMEWORK_DIR="${SWINGAME_DIR}/CoreSDK/lib/mac"
    TO_LIB_DIR="${SOURCE_DIST_DIR}/lib"
elif [ "$OS" = "$WIN" ]; then
    FROM_LIB_DIR="${SWINGAME_DIR}/CoreSDK/lib/win"
    TO_LIB_DIR="${SOURCE_DIST_DIR}/lib"
fi

#
# Step 6: Prepare directories
#
if [ ! -d "${SOURCE_DIST_DIR}" ]
then
    rm -rf "${SOURCE_DIST_DIR}"
fi

mkdir -p "${SRC_DIR}"

#
# Step 7: Define Functions
#

# Create the pascal code and c headers
CreateLibrary()
{
    cd "${PYTHON_SCRIPT_DIR}"
    python create_pas_lib.py
    if [ $? != 0 ]; then echo "Error creating Pascal library."; exit 1; fi
    
    cp -p "${C_COMMON_LIB_DIR}/SGSDK.h" "${SRC_DIR}/SGSDK.h"
    if [ $? != 0 ]; then echo "Error copying header."; exit 1; fi
    
    cp -p "${C_COMMON_LIB_DIR}/Types.h" "${SRC_DIR}/Types.h"
    if [ $? != 0 ]; then echo "Error copying header."; exit 1; fi
}

source "${APP_PATH}/inc/copy_without_svn.sh"

#
# Step 8: Create code and copy to destination
#
echo "--------------------------------------------------"
echo "          Creating Source Template"
echo "              for $OS"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
echo "  Build options are ${EXTRA_OPTS}"
echo "  Copying"
echo "    from ${SDK_SRC}"
echo "    and ${SDK_LIB_SRC}"
echo "    to ${SOURCE_DIST_DIR}"

if [ "$OS" = "$MAC" ]; then
    echo "  Copying Frameworks"
    echo "    from ${FRAMEWORK_DIR}"
    echo "      to ${TO_LIB_DIR}"
    mkdir -p ${TO_LIB_DIR}
elif [ "$OS" = "$WIN" ]; then
    echo "  Copying libraries"
    echo "    from ${FROM_LIB_DIR}"
    echo "      to ${TO_LIB_DIR}"
    mkdir -p ${TO_LIB_DIR}
fi
echo "--------------------------------------------------"

rm ${SRC_DIR}/*.pas 2>>/dev/null

echo "  Creating Pascal library interface"
CreateLibrary

echo "  Copying Pascal source"
find ${SDK_SRC} -maxdepth 1 -path \*.pas -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi

find ${SDK_SRC} -maxdepth 1 -path \*.inc -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi

find ${SDK_LIB_SRC} -maxdepth 1 -path \*.pas -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi

find ${SDK_LIB_SRC} -maxdepth 1 -path \*.inc -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi

echo "  Copying build script"
cp -p "${SOURCE_TEMPLATE_DIR}"/build.sh "${SOURCE_DIST_DIR}"
chmod a+x "${SOURCE_DIST_DIR}"/build.sh

if [ "$OS" = "$MAC" ]; then
    echo "  Copying Frameworks"
    rm -Rf "${TO_LIB_DIR}"/*
    copyFrameworksWithoutSVN ${FRAMEWORK_DIR} ${TO_LIB_DIR}
    if [ $? != 0 ]; then echo "Error copying frameworks."; exit 1; fi
elif [ "$OS" = "$WIN" ]; then
    echo "  Copying libraries"
    rm -Rf "${TO_LIB_DIR}"/*
    copyWithoutSVN ${FROM_LIB_DIR} ${TO_LIB_DIR}
    if [ $? != 0 ]; then echo "Error copying libraries."; exit 1; fi
fi

echo "  Finished"
if [ $BUILD = "Y" ]; then
    echo "  Preparing to build..."
fi
echo "--------------------------------------------------"

if [ $BUILD = "Y" ]; then
    echo
    ${SOURCE_DIST_DIR}/build.sh -c
    echo
    ${SOURCE_DIST_DIR}/build.sh ${EXTRA_OPTS}
fi