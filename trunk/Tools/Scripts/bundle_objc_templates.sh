#!/bin/bash

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

#if [ ! "$OS" = "$MAC" ]; then
#    echo "Objective C is only for MacOS (atm)"
#    exit 1
#fi

#
# Step 3: Set the paths to local variables
#
source "${APP_PATH}/inc/base_template_dirs.sh"

# Remove all previously generated files
find ${GENERATED_DIR} -type f ! -path \*.svn\* -exec rm -f {} \;

#
# Step 4: Set up array of files to copy
#
COPY_LIST=( "Command line gcc,${GCC_OBJC_TEMPLATE_DIR},${GCC_OBJC_DIST_DIR}" )

if [ "$OS" = "$MAC" ]; then
    COPY_LIST=( "${COPY_LIST[@]}" "XCode 3,${XCODE_OBJC_TEMPLATE_DIR},${XCODE_OBJC_DIST_DIR}")
    
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
    echo "  ... Creating C library code"
    python create_c_library.py
    echo "  ... Creating Objective C library code"
    python create_objc_library.py
    
    # cp -p "${COMMON_C_TEMPLATE_DIR}"/lib/SGSDK.h "${COMMON_OBJC_TEMPLATE_DIR}/lib"
    # cp -p "${COMMON_C_TEMPLATE_DIR}"/lib/Types.h "${COMMON_OBJC_TEMPLATE_DIR}/lib"
    
    cp -p "${C_GENERATED_DIR}"/lib/*.h "${OBJC_GENERATED_DIR}/lib"
    cp -p "${C_GENERATED_DIR}"/lib/*.c "${OBJC_GENERATED_DIR}/lib"
    
    cp -p "${COMMON_C_TEMPLATE_DIR}"/lib/*.h "${OBJC_GENERATED_DIR}/lib"
    cp -p "${COMMON_C_TEMPLATE_DIR}"/lib/*.c "${OBJC_GENERATED_DIR}/lib"
    
    #Create SwinGame.h
    cd "${COMMON_OBJC_TEMPLATE_DIR}"/lib
    ls *.h | grep -v SGSDK.* | awk '{ printf("#import \"%s\"\n", $1); }' > "${OBJC_GENERATED_DIR}"/lib/SwinGame.h
    cd "${OBJC_GENERATED_DIR}"/lib
    ls *.h | grep -v SGSDK.* | awk '{ printf("#import \"%s\"\n", $1); }' >> SwinGame.h
}

source ${APP_PATH}/inc/copy_without_svn.sh
source ${APP_PATH}/inc/dist_dir.sh

#
# Step 6: Create c library and copy
#
echo "--------------------------------------------------"
echo "      Creating SwinGame Objective C Templates"
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
CreateCCode

DoDist "${COPY_LIST}" "${OBJC_DIST_DIR}" "${OBJC_GENERATED_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_OBJC_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"
