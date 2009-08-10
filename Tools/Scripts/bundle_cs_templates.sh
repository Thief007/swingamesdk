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

#
# Step 3: Set the paths to local variables
#
PYTHON_SCRIPT='create_csharp_library.py'
source "${APP_PATH}/inc/base_template_dirs.sh"

#
# Step 4: Set up array of files to copy
#
if [ "$OS" = "$WIN" ]; then
    COPY_LIST=( "VS08,${VS08_TEMPLATE_DIR},${VS08_DIST_DIR}" )
    #COPY_LIST=( "${COPY_LIST[@]}" "Command Line,${CL_TEMPLATE_DIR},${CL_DIST_DIR}")
else
    COPY_LIST=( "Mono,${MONO_TEMPLATE_DIR},${MONO_DIST_DIR}" )
fi

#
# Step 5: Declare functions
#
source ${APP_PATH}/inc/copy_without_svn.sh
source ${APP_PATH}/inc/dist_dir.sh

CreateCSCode()
{
    cd ${PYTHON_SCRIPT_DIR}
    python ${PYTHON_SCRIPT}
}

#
# Step 6: Create c# library and copy
#
echo "--------------------------------------------------"
echo "      Creating $OS SwinGame C# Templates"
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
echo "  ... Creating C# library code"
CreateCSCode

DoDist "${COPY_LIST}" "${CS_DIST_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_CS_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"
