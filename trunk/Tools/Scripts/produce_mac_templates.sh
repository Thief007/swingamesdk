#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#./clean_all.sh
./create_library.sh
./bundle_c_templates.sh
#./bundle_cs_templates.sh
./bundle_objc_templates.sh
#./bundle_pas_templates.sh

#
# Create XCode packages
#

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

DIST_DIR="${SWINGAME_DIR}/Dist"

OBJC_DIST_DIR="${DIST_DIR}/ObjC"
C_DIST_DIR="${DIST_DIR}/C"

XCODE_OBJC_DIST_DIR="${OBJC_DIST_DIR}/xcode 3"
XCODE_C_DIST_DIR="${C_DIST_DIR}/xcode 3"

TMP_DIR=${SWINGAME_DIR}/tmp
PKG_TMP_DIR="${TMP_DIR}/Pkgs/"

PKG_XCODE_OBJC_DIR="${PKG_TMP_DIR}/SwinGame/SwinGame - Objective C"
PKG_XCODE_C_DIR="${PKG_TMP_DIR}/SwinGame/SwinGame - C"

COPY_LIST=( "Objective C,${XCODE_OBJC_DIST_DIR},${PKG_XCODE_OBJC_DIR}" )
COPY_LIST=( "${COPY_LIST[@]}" "C,${XCODE_C_DIST_DIR},${PKG_XCODE_C_DIR}")

# 1: Copy all files to related directories
source ${APP_PATH}/inc/copy_without_svn.sh

rm -rf ${PKG_TMP_DIR}
rm "${DIST_DIR}/SwinGame XCode Templates.pkg"

DoCopy "${COPY_LIST}"

echo " ... Creating packages"
/Developer/usr/bin/packagemaker --doc "${APP_PATH}/pkgdocs/SwinGame.pmdoc" --version 3.0 --out "${DIST_DIR}/SwinGame XCode Templates.pkg"
echo " ... Done"