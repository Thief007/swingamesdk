#!/bin/bash

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

echo ${APP_PATH}/inc/version.sh

source "${APP_PATH}/inc/version.sh"
source "${APP_PATH}/inc/base_template_dirs.sh"

echo -n "Is ${SG_VERSION} the correct version number? [yn]: "
read answer
if [ "a${answer}a" != "aya" ] ; then
    echo "Please update version.sh and retry."
    exit 1
fi

echo -n "Clean all and recreate templates? [yn]: "
read answer

if [ "a${answer}a" == "aya" ] ; then
    ./clean_all.sh
    ./create_library.sh
    ./bundle_c_templates.sh
    ./bundle_cs_templates.sh
    #./bundle_objc_templates.sh
    ./bundle_pas_templates.sh
    echo
    echo
fi

echo "--------------------------------------------------"
echo "          Producing Linux Distribution Files"
echo "--------------------------------------------------"


#
# Create version dir on mercury
#

MERCURY_BASE_INST_DIR="/home/acad/acain/www/htdocs/media/SwinGame"
MERCURY_INST_DIR="${MERCURY_BASE_INST_DIR}/SwinGame $SG_VERSION"
MERCURY_INST_DIR_NO_SPACE=`echo ${MERCURY_INST_DIR} | awk '{gsub(/[ \t]/,"\\\\ ");print}'`

echo " - Creating destination on server"
ssh mercury.it.swin.edu.au "mkdir -p \"${MERCURY_INST_DIR}\""

echo " - Saving version name on server"
ssh mercury.it.swin.edu.au "echo ${SG_VERSION_WEB} > \"${MERCURY_INST_DIR}/version.txt\""




LINUX_DMG_LIST=( "C GCC,${GCC_C_DIST_DIR},Project Template")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "C GPP,${GPP_C_DIST_DIR}")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "C CodeBlocks,${CODEBLOCKS_C_DIST_DIR},")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "Mono C#,${MONO_DIST_DIR},Project Template")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "FPC,${FPC_PAS_DIST_DIR},Project Template")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "Source,${SOURCE_DIST_DIR},SwinGame")

source ${APP_PATH}/inc/copy_without_svn.sh

TAR_TMP_DIR="${TMP_DIR}/Tars"
rm -rf ${TAR_TMP_DIR}

echo "  ... Creating tar gz files"
for arg in "${LINUX_DMG_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    subdir=`echo $arg | awk -F"," '{print $3}'`
    
    TAR_ROOT="${TAR_TMP_DIR}/SwinGame ${SG_VERSION} ${name}"
    
    if [ "aa" == "a${subdir}a" ] ; then
        TAR_BASE_DIR="${TAR_ROOT}"
    else
        TAR_BASE_DIR="${TAR_ROOT}/${subdir}"
    fi
    
    create_from="${TAR_TMP_DIR}/SwinGame ${SG_VERSION} ${name}"
    
    to="${DIST_DIR}/SwinGame ${SG_VERSION} ${name}.tar"
    echo "  ... ${to}"
    
    if [ -f "${to}" ] ; then
       rm "${to}" 
    fi
    
    mkdir -p "${TAR_BASE_DIR}"
    cp -r "${from}"/* "${TAR_BASE_DIR}"
    
    cd "${TAR_ROOT}"
    tar -cf "${to}" *
    gzip -f -q "${to}"
    
    echo "      Copying to server"
    scp "${to}.gz" mercury.it.swin.edu.au:"${MERCURY_INST_DIR_NO_SPACE}/"
    
    rm -rf "${TAR_BASE_DIR}"
done

rm -rf ${TAR_TMP_DIR}

echo " ... Done"

