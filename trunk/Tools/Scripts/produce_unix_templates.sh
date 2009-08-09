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
fi

LINUX_DMG_LIST=( "C GCC,${GCC_C_DIST_DIR},Project Template")
LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "C CodeBlocks,${CODEBLOCKS_C_DIST_DIR},")
#LINUX_DMG_LIST=( "${LINUX_DMG_LIST[@]}" "ObjC GCC,${GCC_OBJC_DIST_DIR},Project Template")
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
    
    mkdir -p "${TAR_BASE_DIR}"
    cp -r "${from}/" "${TAR_BASE_DIR}"
    
    cd "${TAR_ROOT}"
    tar -cf "${to}" *
    gzip -f -q "${to}"
    
    rm -rf "${TAR_BASE_DIR}"
done

rm -rf ${PKG_TMP_DIR}
rm -rf ${TAR_TMP_DIR}

echo " ... Done"

