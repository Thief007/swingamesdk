#!/bin/bash

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

source ${APP_PATH}/inc/version.sh

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
    ./bundle_objc_templates.sh
    ./bundle_pas_templates.sh
    ./bundle_docs.sh
    
    echo
    echo
fi

echo "--------------------------------------------------"
echo "          Producing Mac Distribution Files"
echo "--------------------------------------------------"


#
# Create XCode packages
#
source "${APP_PATH}/inc/base_template_dirs.sh"

PKG_TMP_DIR="${TMP_DIR}/Pkgs"
DMG_TMP_DIR="${TMP_DIR}/Dmgs"

PKG_XCODE_OBJC_DIR="${PKG_TMP_DIR}/SwinGame/SwinGame - Objective C"
PKG_XCODE_C_DIR="${PKG_TMP_DIR}/SwinGame/SwinGame - C"

PKG_COPY_LIST=( "Objective C,${XCODE_OBJC_DIST_DIR},${PKG_XCODE_OBJC_DIR}" )
PKG_COPY_LIST=( "${PKG_COPY_LIST[@]}" "C,${XCODE_C_DIST_DIR},${PKG_XCODE_C_DIR}")

MAC_DMG_LIST=( "C GCC,${GCC_C_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "C GPP,${GPP_C_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "C XCode,${XCODE_C_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "ObjC GCC,${GCC_OBJC_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "ObjC XCode,${XCODE_OBJC_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "Mono C#,${MONO_DIST_DIR}")
MAC_DMG_LIST=( "${MAC_DMG_LIST[@]}" "FPC,${FPC_PAS_DIST_DIR}")

source ${APP_PATH}/inc/copy_without_svn.sh

XCODE_PKG_NAME="${DIST_DIR}/SwinGame ${SG_VERSION} XCode Templates.pkg"

# Step 1: Remove old packages and package dirs
rm -rf ${PKG_TMP_DIR}
rm -rf ${DMG_TMP_DIR}
if [ -f "$XCODE_PKG_NAME" ] ; then
    rm -f "${XCODE_PKG_NAME}"
fi

# Step 2: Copy all files to related directories
echo "  ... Copying files to pkg directory"
COPY_LIST=( "${PKG_COPY_LIST[@]}" )
DoCopy "${COPY_LIST}"

# Step 3: Create packages
echo "  ... Creating packages"
/Developer/usr/bin/packagemaker --doc "${APP_PATH}/pkgdocs/SwinGame.pmdoc" --version "${SG_VERSION}" --out "${XCODE_PKG_NAME}"


echo "  ... Creating DMGs"
for arg in "${MAC_DMG_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    
    DMG_BASE_DIR="${DMG_TMP_DIR}/SwinGame ${SG_VERSION} ${name}/Project Template"
    create_from="${DMG_TMP_DIR}/SwinGame ${SG_VERSION} ${name}"
    
    to="${DIST_DIR}/SwinGame ${SG_VERSION} ${name}.dmg"
    echo "  ... ${to}"
    
    mkdir -p "${DMG_BASE_DIR}"
    cp -r "${from}/" "${DMG_BASE_DIR}"
    hdiutil create -quiet -ov -fs HFS+ -volname "${name}" - -srcfolder "${create_from}" "${to}"
    rm -rf "${DMG_BASE_DIR}"
done

rm -rf ${PKG_TMP_DIR}
rm -rf ${DMG_TMP_DIR}

echo " ... Done"

