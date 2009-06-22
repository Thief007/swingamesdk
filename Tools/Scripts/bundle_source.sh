# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
SOURCE_TEMPLATE_DIR="${TEMPLATE_DIR}/Source"

DIST_DIR="${SWINGAME_DIR}/Dist"
SOURCE_DIST_DIR="${DIST_DIR}/Source"

SRC_DIR="${SOURCE_DIST_DIR}/src"
SDK_SRC="${SWINGAME_DIR}/CoreSDK/src"

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

CreateLibrary()
{
    cd ${PYTHON_SCRIPT_DIR}
    python create_pas_lib.py
    cp ${PYTHON_SCRIPT_DIR}/out/SGSDK_Lib.h ${SRC_DIR}/SGSDK.h
    cp ${PYTHON_SCRIPT_DIR}/out/sgTypes.h ${SRC_DIR}/sgTypes.h
}

echo "--------------------------------------------------"
echo "          Creating Source Template"
echo "              for Mac OS X and Linux"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
echo "  Build options are ${EXTRA_OPTS}"
echo "  Copying"
echo "    from ${SDK_SRC}"
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

echo "  Copying build script"
cp -p "${SOURCE_TEMPLATE_DIR}"/build.sh "${SOURCE_DIST_DIR}"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    echo "  Copying frameworks"
    rm -Rf "${LIB_DIR}"/*
    cp -pRf "${FRAMEWORK_DIR}"/*.framework "${LIB_DIR}"
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