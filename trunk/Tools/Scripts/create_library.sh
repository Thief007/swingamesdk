#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
SWINGAME_PATH=`cd ../..; pwd`
OUT_DIR="${SWINGAME_PATH}/bin/lib"
TMP_DIR="${SWINGAME_PATH}/tmp"
SDK_SRC_DIR="${SWINGAME_PATH}/CoreSDK/src"
FRAMEWORK_DIR="${SWINGAME_PATH}/CoreSDK/lib/mac"
LOG_FILE="${APP_PATH}/out.log"

PYTHON_SCRIPT_DIR=${SWINGAME_PATH}/Tools/SGWrapperGen

VERSION=3.0

FPC_BIN=`which fpc`

EXTRA_OPTS="-O3 -Sewn -vwn"

CLEAN="N"

DoExitAsm ()
{ 
    echo "An error occurred while assembling $1" 
    exit 1 
}

Usage()
{
    echo "Usage: [-c] [-h] version"
    echo 
    echo "Creates and Compiles the native SGSDK library."
    echo "Libraries are copied to $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -h   Show this help message "
    exit 0
}

CleanTmp()
{
    if [ -d "${TMP_DIR}" ]
    then
        rm "${TMP_DIR}"/*.o 2>> /dev/null
        rm "${TMP_DIR}"/*.s 2>> /dev/null
        rm "${TMP_DIR}"/*.ppu 2>> /dev/null
        rm "${TMP_DIR}"/link.res 2>> /dev/null
        rm "${TMP_DIR}"/ppas.sh 2>> /dev/null
        rm "${TMP_DIR}"/linksyms.fpc 2>> /dev/null
    else
        mkdir "${TMP_DIR}" 
    fi
}

CreateLibrary()
{
    cd ${PYTHON_SCRIPT_DIR}
    python create_pas_lib.py
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doCompile()
{
    CleanTmp
    
    ${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"${TMP_DIR}" -FU"${TMP_DIR}" -s ${SDK_SRC_DIR}/sgsdk1.pas >> ${LOG_FILE}
    
    if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${LOG_FILE}; exit 1; fi

    #Assemble all of the .s files
    echo "  ... Assembling library for $1"
    
    for file in `find ${TMP_DIR} | grep [.]s$`
    do
        /usr/bin/as -o ${file%.s}.o $file -arch $1
        if [ $? != 0 ]; then DoExitAsm $file; fi
        rm $file
    done
    

    echo "  ... Linking Library"
    FRAMEWORKS=`ls ${FRAMEWORK_DIR} | awk -F . '{print "-framework "$1}'`

    /usr/bin/libtool -install_name "@executable_path/../Frameworks/SGSDK.framework/Versions/${VERSION}/SGSDK"  -arch_only ${1} -dynamic -L"${TMP_DIR}" -F${FRAMEWORK_DIR} -search_paths_first -multiply_defined suppress -o "${TMP_DIR}/libSGSDK${1}.dylib" ${FRAMEWORKS} -current_version ${VERSION} -read_only_relocs suppress `cat ${TMP_DIR}/link.res` -framework Cocoa
    if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
    
    CleanTmp
}

# 
# Create fat dylib
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${TMP_DIR}/libSGSDK${1}.dylib" -arch ${2} "${TMP_DIR}/libSGSDK${2}.dylib" -output "${OUT_DIR}/libSGSDK.dylib" -create
    
    rm -rf "${TMP_DIR}/libSGSDK${1}.dylib"
    rm -rf "${TMP_DIR}/libSGSDK${2}.dylib"
}

#
# Create Mac Framework
#
doCreateFramework()
{
    echo "  ... Creating Framework version ${VERSION}"
    
    VERSION_DIR="${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
    HEADER_DIR="${VERSION_DIR}/Headers"
    RESOURCES_DIR="${VERSION_DIR}/Resources"
    CURRENT_DIR="${OUT_DIR}/SGSDK.framework/Versions/Current"
    
    if [ ! -d "${OUT_DIR}/SGSDK.framework" ]
    then
        mkdir "${OUT_DIR}/SGSDK.framework"
        mkdir "${OUT_DIR}/SGSDK.framework/Versions"
    else
        rm -rf "${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
    fi
    
    
    mkdir ${VERSION_DIR}
    mkdir ${HEADER_DIR}
    mkdir ${RESOURCES_DIR}
    
    mv "${PYTHON_SCRIPT_DIR}/out/SGSDK_lib.h" "${HEADER_DIR}/SGSDK.h"
    mv "${OUT_DIR}/libSGSDK.dylib" "${VERSION_DIR}/SGSDK"
    
    echo "<?xml version='1.0' encoding='UTF-8'?>\
    <!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
    <plist version=\"1.0\">\
    <dict>\
            <key>CFBundleName</key>\
            <string>SGSDK</string>\
            <key>CFBundleIdentifier</key>\
            <string>au.edu.swinburne.SGSDK</string>\
            <key>CFBundleVersion</key>\
            <string>${VERSION}</string>\
            <key>CFBundleDevelopmentRegion</key>\
            <string>English</string>\
            <key>CFBundleExecutable</key>\
            <string>SGSDK</string>\
            <key>CFBundleInfoDictionaryVersion</key>\
            <string>6.0</string>\
            <key>CFBundlePackageType</key>\
            <string>FMWK</string>\
            <key>CFBundleSignature</key>\
            <string>SWIN</string>\
    </dict>\
    </plist>" >> "${RESOURCES_DIR}/Info.plist"
    
    cd "${OUT_DIR}/SGSDK.framework"    
    ln -f -s "./${VERSION}" "./Versions/Current"
    ln -f -s "./Versions/Current/Resources" "./Resources"
    ln -f -s "./Versions/Current/Headers" "./Headers"
    ln -f -s "./Versions/Current/SGSDK" "./SGSDK"
}

while getopts chd o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    d)  EXTRA_OPTS="-vwn -gw";;
    esac
done

shift $((${OPTIND}-1))

if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


if [ $CLEAN = "N" ]
then
    if [ ! -d "${OUT_DIR}" ]
    then
        mkdir -p "${OUT_DIR}"
    fi
    
    if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
    then
        echo "--------------------------------------------------"
        echo "          Creating SwinGame Dynamic Library"
        echo "                 for Mac OS X"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Copying Frameworks from ${FRAMEWORK_DIR}"
        echo "  Compiling with $EXTRA_OPTS"
        echo "--------------------------------------------------"
        
        echo "  ... Creating Pascal Library"
        CreateLibrary >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error creating pascal library SGSDK"; cat ${LOG_FILE}; exit 1; fi
            
        echo "  ... Compiling Library"
        FPC_BIN=`which ppcppc`
        doCompile "ppc"
        
        FPC_BIN=`which ppc386`
        doCompile "i386"
        
        doLipo "i386" "ppc"
        
        doCreateFramework
    else
        echo "--------------------------------------------------"
        echo "          Creating SwinGame Dynamic Library"
        echo "                 for Linux"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Compiling with $EXTRA_OPTS"
        echo "--------------------------------------------------"
        
        echo "  ... Creating Pascal Library"
        CreateLibrary >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error creating pascal library SGSDK"; cat ${LOG_FILE}; exit 1; fi
        
        echo "  ... Compiling Library"
        fpc -Mdelphi $EXTRA_OPTS -FE"${OUT_DIR}" ${SDK_SRC_DIR}/sgsdk1.pas >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${LOG_FILE}; exit 1; fi
        
        CleanTmp
    fi
else
    CleanTmp
    rm -rf "${OUT_DIR}"
    mkdir "${OUT_DIR}"
    echo    ... Cleaned
fi

echo "  Finished"
echo "--------------------------------------------------"