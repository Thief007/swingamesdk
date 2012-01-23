#!/bin/bash

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
source "${APP_PATH}/inc/base_template_dirs.sh"
PYTHON_SCRIPT='create_pas_lib.py'
PYTHON_UNIT_SCRIPT='lang_pas_unit.py'


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

CreatePasCode()
{
    cd ${PYTHON_SCRIPT_DIR}
    echo "  Creating SwinGame Library Code"
    python ${PYTHON_SCRIPT}
    echo "  Creating SwinGame Unit Code"
    python ${PYTHON_UNIT_SCRIPT}
}

#
# Step 6: Copy Pascal Library
#
echo "--------------------------------------------------"
echo "      Creating $OS SwinGame Pascal Templates"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "
ListDists "${COPY_LIST}"
echo "--------------------------------------------------"

CreatePasCode

copyWithoutSVN "${SOURCE_DIST_DIR}/src" "${PAS_GENERATED_DIR}/lib"

DoDist "${COPY_LIST}" "${PAS_DIST_DIR}" "${PAS_GENERATED_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_PAS_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"
