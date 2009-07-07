#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
OUT_DIR="${APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"
LIB_DIR="${APP_PATH}/lib"
LOG_FILE="${APP_PATH}/out.log"

GMCS_FLAGS="-target:winexe -r:System.Drawing -r:Microsoft.VisualBasic"
CS_FLAGS="-optimize+"
SG_INC="-I${APP_PATH}/lib/"

if [ -d "/c/Windows" ]; then
    GMCS_BIN=`which csc`
else
    GMCS_BIN=`which gmcs`
fi

GAME_NAME=${APP_PATH##*/}

CLEAN="N"

Usage()
{
    echo "Usage: [-c] [-h] [-d] [name]"
    echo 
    echo "Compiles your game into an executable application."
    echo "Output is located in $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -d   Debug build"
    echo " -h   Show this help message "
    exit 0
}

while getopts chd o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    d)  CS_FLAGS="-debug -define:DEBUG" ;;
    ?)  Usage
    esac
done

shift $((${OPTIND}-1))

if [ "a$1a" != "aa" ]; then
    GAME_NAME=$1
fi

if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


CleanTmp()
{
    if [ -d "${TMP_DIR}" ]
    then
        rm -rf "${TMP_DIR}"
    fi
    mkdir "${TMP_DIR}"
}

doMacPackage()
{
    GAMEAPP_PATH="${OUT_DIR}/${GAME_NAME}.app"
    if [ -d "${GAMEAPP_PATH}" ] 
    then
        echo "  ... Removing old application"
        rm -rf "${GAMEAPP_PATH}"
    fi
    
    echo "  ... Creating Application Bundle"
    
    macpack -m winforms -n "${GAME_NAME}" -o "${OUT_DIR}" "${OUT_DIR}/${GAME_NAME}.exe"
    # mkdir "${GAMEAPP_PATH}"
    # mkdir "${GAMEAPP_PATH}/Contents"
    # mkdir "${GAMEAPP_PATH}/Contents/MacOS"
    # mkdir "${GAMEAPP_PATH}/Contents/Resources"
    mkdir "${GAMEAPP_PATH}/Contents/Frameworks"
    
    echo "  ... Adding Private Frameworks"
    cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"
    
    pushd . >> /dev/null
    cd "${GAMEAPP_PATH}/Contents/Resources"
    ln -s ../Frameworks/SGSDK.framework/SGSDK libSGSDK.dylib
    ln -s ../Frameworks ./Frameworks #Silly macpac uses ./bin folder
    popd >> /dev/null
    
    rm -f "${OUT_DIR}/${GAME_NAME}.exe"
    
    if [ -f "${EXECUTABLE_NAME}.mdb" ]
    then
        echo "  ... Adding Debug Information"
        mv "${EXECUTABLE_NAME}.mdb" "${PRODUCT_NAME}.app/Contents/Resources"
    fi
    
    echo "  ... Adding Application Information"
    echo "<?xml version='1.0' encoding='UTF-8'?>\
    <!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
    <plist version=\"1.0\">\
    <dict>\
            <key>CFBundleDevelopmentRegion</key>\
            <string>English</string>\
            <key>CFBundleExecutable</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundleIconFile</key>\
            <string>${ICON}</string>\
            <key>CFBundleIdentifier</key>\
            <string>au.edu.swinburne.${GAME_NAME}</string>\
            <key>CFBundleInfoDictionaryVersion</key>\
            <string>6.0</string>\
            <key>CFBundleName</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundlePackageType</key>\
            <string>APPL</string>\
            <key>CFBundleSignature</key>\
            <string>SWIN</string>\
            <key>CFBundleVersion</key>\
            <string>1.0</string>\
            <key>CSResourcesFileMapped</key>\
            <true/>\
    </dict>\
    </plist>" >> "${GAMEAPP_PATH}/Contents/Info.plist"
    
    echo "APPLSWIN" >> "${GAMEAPP_PATH}/Contents/PkgInfo"
    
    RESOURCE_DIR="${GAMEAPP_PATH}/Contents/Resources"
}

doCompile()
{
    CleanTmp
    
    if [ ! -d ${OUT_DIR} ]; then
        mkdir -p ${OUT_DIR}
    fi
    
    ${GMCS_BIN} ${GMCS_FLAGS} ${CS_FLAGS} -out:"${OUT_DIR}/${GAME_NAME}.exe" `find ${APP_PATH} -mindepth 2 | grep [.]cs$` >> ${LOG_FILE}
    if [ $? != 0 ]; then echo "Error compiling."; exit 1; fi
    
    CleanTmp
}

doLinuxPackage()
{
    RESOURCE_DIR="${APP_PATH}/bin/"
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

#
# Copy Resources from standard location to $RESOURCE_DIR
#
doCopyResources()
{
    echo "  ... Copying Resources into $GAME_NAME"
    
    copyWithoutSVN "${APP_PATH}/Resources" "${RESOURCE_DIR}"
}


if [ $CLEAN = "N" ]
then
    if [ ! -d "${OUT_DIR}" ]
    then
        mkdir -p "${OUT_DIR}"
    fi
    
    if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
    then
        echo "--------------------------------------------------"
        echo "          Creating $GAME_NAME"
        echo "                 for Mac OS X"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Compiler flags ${CS_FLAGS}"
        echo "--------------------------------------------------"
        echo "  ... Creating ${GAME_NAME}"
        
        doCompile
        doMacPackage
    else
        echo "--------------------------------------------------"
        echo "          Creating $GAME_NAME"
        echo "                 for Linux"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "--------------------------------------------------"
        echo "  ... Creating C wrapper library"
        echo "  Compiler flags ${CS_FLAGS}"
        CreateCWrapper >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error creating c wrapper library"; cat ${LOG_FILE}; exit 1; fi
        
        echo "  ... Compiling Library"
        doLinuxCompile
        
        doLinuxPackage
    fi
    
    doCopyResources
else
    CleanTmp
    rm -rf "${OUT_DIR}"
    mkdir "${OUT_DIR}"
    echo    ... Cleaned
fi

#remove temp files on success
rm -f ${LOG_FILE} 2>> /dev/null
rm -rf ${TMP_DIR} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"