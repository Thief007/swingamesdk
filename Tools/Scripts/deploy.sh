#!/bin/bash

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

source "${APP_PATH}/inc/version.sh"
source "${APP_PATH}/inc/base_template_dirs.sh"

echo -n "Is ${SG_VERSION} the correct version number? [yn]: "
read answer
if [ "a${answer}a" != "aya" ] ; then
    echo "Please update version.sh and retry."
    exit 1
fi

echo "--------------------------------------------------"
echo "          Deploying Distribution Files"
echo "--------------------------------------------------"


#
# Create version dir on mercury
#

MERCURY_BASE_INST_DIR="/home/acad/acain/www/htdocs/media/SwinGame"
MERCURY_INST_DIR="${MERCURY_BASE_INST_DIR}/SwinGame $SG_VERSION"
MERCURY_INST_DIR_NO_SPACE=`echo ${MERCURY_INST_DIR} | awk '{gsub(/[ \t]/,"\\\\ ");print}'`

echo " - Creating destination on server"
ssh acain@mercury.it.swin.edu.au "mkdir -p \"${MERCURY_INST_DIR}\""

echo " - Saving version name on server"
ssh acain@mercury.it.swin.edu.au "echo ${SG_VERSION_WEB} > \"${MERCURY_INST_DIR}/version.txt\""


DEPLOY_LIST=( "SwinGame ${SG_VERSION} C GCC.zip" )
DEPLOY_LIST=( "${DEPLOY_LIST[@]}" "SwinGame ${SG_VERSION} C CodeBlocks.zip" )
DEPLOY_LIST=( "${DEPLOY_LIST[@]}" "SwinGame ${SG_VERSION} FPC.zip" )
DEPLOY_LIST=( "${DEPLOY_LIST[@]}" "SwinGame ${SG_VERSION} C# Template Installer.vsi" )
DEPLOY_LIST=( "${DEPLOY_LIST[@]}" "SwinGame ${SG_VERSION} VB Template Installer.vsi" )

echo "  ... Copying files to server"
for arg in "${DEPLOY_LIST[@]}"; do
    name=$arg
    
    to="${DIST_DIR}/$name"
    echo "  ... ${to}"
    
    if [ ! -f "${to}" ] ; then
		echo 'Unable to find ${to}'
		exit -1
    fi
    
    scp "${to}" acain@mercury.it.swin.edu.au:"${MERCURY_INST_DIR_NO_SPACE}/"
done

echo " ... Done"

