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
PYTHON_SCRIPT='create_csharp_library.py'

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
DIST_DIR="${SWINGAME_DIR}/Dist"

CS_TEMPLATE_DIR="${TEMPLATE_DIR}/CSharp"
CS_DIST_DIR="${DIST_DIR}/CSharp"

COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"
COMMON_CS_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/Common"

MONO_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/Mono"
MONO_DIST_DIR="${CS_DIST_DIR}/Mono"

VS05_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/VS05"
VS05_DIST_DIR="${CS_DIST_DIR}/VS05"

VS08_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/VS08"
VS08_DIST_DIR="${CS_DIST_DIR}/VS08"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

COPY_LIST=( "Mono,${MONO_TEMPLATE_DIR},${MONO_DIST_DIR}" )

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

CreateCSCode()
{
    cd ${PYTHON_SCRIPT_DIR}
    python ${PYTHON_SCRIPT}
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

if [ -d $CS_DIST_DIR ]; then
    echo "  ... Removing old C# dist"
    rm -rf ${CS_DIST_DIR}
fi

#Step 2: Create dists
echo "  ... Creating dist directories"
for arg in "${COPY_LIST[@]}"; do
    to=`echo $arg | awk -F"," '{print $3}'`
    
    mkdir -p "${to}"
done

#Step 3: Create the C# lib code
echo "  ... Creating C# library code"
CreateCSCode

#Step 4: Copy common files to all
echo "  ... Copying files"
for arg in "${COPY_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    to=`echo $arg | awk -F"," '{print $3}'`
    
    echo -n "  ... Copying to $name"
    
    copyWithoutSVN "$COMMON_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$COMMON_CS_TEMPLATE_DIR" "$to"
    copyWithoutSVN "$from" "$to"
    chmod a+x ${to}/*.sh
    
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
