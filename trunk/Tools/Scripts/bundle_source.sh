# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

SDK_SRC="${SWINGAME_DIR}/CoreSDK/src"
SDK_LIB_SRC="${SWINGAME_DIR}/CoreSDK/libsrc"

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
DIST_DIR="${SWINGAME_DIR}/Dist"

SOURCE_TEMPLATE_DIR="${TEMPLATE_DIR}/Source"

SOURCE_DIST_DIR="${DIST_DIR}/Source"

SRC_DIR="${SOURCE_DIST_DIR}/src"

C_TEMPLATE_DIR="${TEMPLATE_DIR}/C"
C_COMMON_LIB_DIR="${C_TEMPLATE_DIR}/common/lib"

PYTHON_SCRIPT_DIR=${SWINGAME_DIR}/Tools/SGWrapperGen

BUILD="N"
EXTRA_OPTS=""

while getopts bid o
do
    case "$o" in
    b)  BUILD="Y";;
    d)  BUILD="Y"
        EXTRA_OPTS="${EXTRA_OPTS} -d";;
    i)  EXTRA_OPTS="${EXTRA_OPTS} -i local" ;;
    esac
done

shift $((${OPTIND}-1))

if [ ! -d ${SOURCE_DIST_DIR} ]
then
    rm -rf ${SOURCE_DIST_DIR}
fi

mkdir -p ${SRC_DIR}

#Functions

CreateLibrary()
{
    cd ${PYTHON_SCRIPT_DIR}
    python create_pas_lib.py
    if [ $? != 0 ]; then echo "Error creating Pascal library."; exit 1; fi
    cp -p ${C_COMMON_LIB_DIR}/SGSDK.h ${SRC_DIR}/SGSDK.h
    if [ $? != 0 ]; then echo "Error copying header."; exit 1; fi
    cp -p ${C_COMMON_LIB_DIR}/Types.h ${SRC_DIR}/Types.h
    if [ $? != 0 ]; then echo "Error copying header."; exit 1; fi
}

copyFrameworksWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . ! -path \*.svn\* ! -path \*/. -mindepth 1 -type d -path \*.framework\* -exec mkdir "${TO_DIR}/{}" \;
    # Copy files
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;

}

# Steps...

echo "--------------------------------------------------"
echo "          Creating Source Template"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
echo "  Build options are ${EXTRA_OPTS}"
echo "  Copying"
echo "    from ${SDK_SRC}"
echo "    and ${SDK_LIB_SRC}"
echo "    to ${SOURCE_DIST_DIR}"
if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    FRAMEWORK_DIR="${SWINGAME_DIR}/CoreSDK/lib/mac"
    LIB_DIR="${SOURCE_DIST_DIR}/lib"
    echo "  Copying Frameworks"
    echo "    from ${FRAMEWORK_DIR}"
    echo "      to ${LIB_DIR}"
    mkdir -p ${LIB_DIR}
fi
echo "--------------------------------------------------"

rm ${SRC_DIR}/*.pas 2>>/dev/null

echo "  Creating Pascal library interface"
CreateLibrary

echo "  Copying Pascal source"
find ${SDK_SRC} -maxdepth 1 -path \*.pas -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi
find ${SDK_SRC} -maxdepth 1 -path \*.inc -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi
find ${SDK_LIB_SRC} -maxdepth 1 -path \*.pas -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi
find ${SDK_LIB_SRC} -maxdepth 1 -path \*.inc -exec cp -p {} ${SRC_DIR} \;
if [ $? != 0 ]; then echo "Error copying source."; exit 1; fi

echo "  Copying build script"
cp -p "${SOURCE_TEMPLATE_DIR}"/build.sh "${SOURCE_DIST_DIR}"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    echo "  Copying frameworks"
    rm -Rf "${LIB_DIR}"/*
    copyFrameworksWithoutSVN ${FRAMEWORK_DIR} ${LIB_DIR}
    if [ $? != 0 ]; then echo "Error copying frameworks."; exit 1; fi
fi

echo "  Finished"
echo "--------------------------------------------------"

if [ $BUILD = "Y" ]
then
    echo
    ${SOURCE_DIST_DIR}/build.sh -c
    echo
    ${SOURCE_DIST_DIR}/build.sh ${EXTRA_OPTS}
fi