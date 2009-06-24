#!/bin/sh

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]; then
    OS="Mac"
else
    OS="Linux"
fi

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

PYTHON_SCRIPT_DIR=${SWINGAME_DIR}/Tools/SGWrapperGen

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
DIST_DIR="${SWINGAME_DIR}/Dist"

PAS_TEMPLATE_DIR="${TEMPLATE_DIR}/Pascal"
PAS_DIST_DIR="${DIST_DIR}/Pascal"

COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"
COMMON_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/Common"

FPC_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/FPC"
FPC_PAS_DIST_DIR="${PAS_DIST_DIR}/FPC"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

COPY_LIST=( "Command line FPC,${FPC_PAS_TEMPLATE_DIR},${FPC_PAS_DIST_DIR}" )

source ${APP_PATH}/bundle_source.sh
echo
echo
echo "--------------------------------------------------"
echo "         Creating SwinGame Pascal Templates"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "

for arg in "${COPY_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    echo "    - $name"
done

echo "--------------------------------------------------"

# Functions...

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . ! -path \*.svn\* ! -path \*/. -mindepth 1 -type d -exec mkdir "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

#Step 1: Delete old dists if they exist

if [ -d $PAS_DIST_DIR ]; then
    echo "  ... Removing old Pascal dist"
    rm -rf ${PAS_DIST_DIR}
fi

#Step 2: Create dists
echo "  ... Creating dist directories"
for arg in "${COPY_LIST[@]}"; do
    to=`echo $arg | awk -F"," '{print $3}'`
    
    mkdir -p "${to}"
done

#Step 3: Copy common files to all
echo "  ... Copying files"
for arg in "${COPY_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    to=`echo $arg | awk -F"," '{print $3}'`
    
    echo -n "  ... Copying to $name"
    
    copyWithoutSVN "$COMMON_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$COMMON_PAS_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$from" "$to"
    copyWithoutSVN "${SOURCE_DIST_DIR}/src" "${to}/lib"
    
    if [ $OS = "Mac" ]; then
        echo " with library"
        #Copy SDL frameworks
        cp -R -p "${SOURCE_DIST_DIR}/lib"/*.framework "${to}/lib"
    else
        echo ""
    fi
done
