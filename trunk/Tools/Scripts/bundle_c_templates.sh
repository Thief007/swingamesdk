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

C_TEMPLATE_DIR="${TEMPLATE_DIR}/C"
C_DIST_DIR="${DIST_DIR}/C"

COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"
COMMON_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/Common"

GCC_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/gcc"
GCC_C_DIST_DIR="${C_DIST_DIR}/gcc"

XCODE_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/xcode 3"
XCODE_C_DIST_DIR="${C_DIST_DIR}/xcode 3"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

COPY_LIST=( "Command line gcc,${GCC_C_TEMPLATE_DIR},${GCC_C_DIST_DIR}" )

if [ $OS = "Mac" ]
then
    COPY_LIST=( "${COPY_LIST[@]}" "XCode 3,${XCODE_C_TEMPLATE_DIR},${XCODE_C_DIST_DIR}")
    
    if [ ! -d "${SOURCE_DIST_DIR}/bin/SGSDK.framework" ]; then
        source ${APP_PATH}/bundle_source.sh -b
        if [ $? != 0 ]; then echo "Error building SGSDK framework"; exit 1; fi
    fi
fi

echo "--------------------------------------------------"
echo "          Creating SwinGame C Templates"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "

for arg in "${COPY_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    echo "    - $name"
done

echo "--------------------------------------------------"

echo " Python scripts at $PYTHON_SCRIPT_DIR"

if [ $OS = "Mac" ]; then
    echo "  Copying Library from Source dist"
fi

echo "--------------------------------------------------"

# Functions...

CreateCCode()
{
    cd ${PYTHON_SCRIPT_DIR}
    python create_c_library.py
}

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

if [ -d $C_DIST_DIR ]; then
    echo "  ... Removing old C dist"
    rm -rf ${C_DIST_DIR}
fi

#Step 2: Create dists
echo "  ... Creating dist directories"
for arg in "${COPY_LIST[@]}"; do
    to=`echo $arg | awk -F"," '{print $3}'`
    
    mkdir -p "${to}"
done

#Step 3: Create the C lib code
echo "  ... Creating C library code"
CreateCCode

#Step 4: Copy common files to all
echo "  ... Copying files"
for arg in "${COPY_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    to=`echo $arg | awk -F"," '{print $3}'`
    
    echo -n "  ... Copying to $name"
    
    copyWithoutSVN "$COMMON_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$COMMON_C_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$from" "$to"
    
    if [ $OS = "Mac" ]; then
        echo " with library"
        #Copy SGSDK framework
        cp -R -p "${SOURCE_DIST_DIR}/bin"/*.framework "${to}/lib"
        #Copy SDL frameworks
        cp -R -p "${SOURCE_DIST_DIR}/lib"/*.framework "${to}/lib"
    else
        echo ""
    fi
done
