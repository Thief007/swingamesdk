#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
OUT_DIR="${APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SDK_SRC_DIR="${APP_PATH}/src"
FRAMEWORK_DIR="${APP_PATH}/lib"
LOG_FILE="${APP_PATH}/tmp/out.log"

EXTRA_OPTS="-O3 -Sewn -vwn"
VERSION=3.0
CLEAN="N"
INSTALL="NO"

FPC_BIN=`which fpc`

Usage()
{
    echo "Usage: [-c] [-d] [-i] [-h] version"
    echo 
    echo "Creates and Compiles the native SGSDK library."
    echo "Libraries are copied to $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -d   Compile with debug symbols"
    echo " -h   Show this help message"
    echo " -i   Install after compiling"
    echo
    echo "Requires:"
    echo " - Free Pascal Compiler"
    if [ ! -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
    then
        echo " - SDL dev"
        echo " - SDL-image dev"
        echo " - SDL-mixer dev"
        echo " - SDL-ttf dev"
        echo " - SDL-gfx dev"
    fi
    exit 0
}

while getopts chdi: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    d)  EXTRA_OPTS="-vwn -gw";;
    i)  INSTALL="$OPTARG";;
    [?]) print >&2 "Usage: $0 [-c] [-d] [-i] [-h] version"
         exit -1;;
    esac
done

shift $((${OPTIND}-1))

if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi



DoExitAsm ()
{ 
    echo "An error occurred while assembling $1" 
    exit 1 
}

CleanTmp()
{
    if [ -d "${TMP_DIR}" ]
    then
        rm -rf "${TMP_DIR}"
    fi
    mkdir "${TMP_DIR}" 
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doMacCompile()
{
    CleanTmp
    
    ${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"${TMP_DIR}" -FU"${TMP_DIR}" -s ${SDK_SRC_DIR}/sgsdk.pas >> ${LOG_FILE}
    
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

    /usr/bin/libtool -install_name "@executable_path/../Frameworks/SGSDK.framework/Versions/${VERSION}/SGSDK"  -arch_only ${1} -dynamic -L"${TMP_DIR}" -F${FRAMEWORK_DIR} -search_paths_first -multiply_defined suppress -o "${OUT_DIR}/libSGSDK${1}.dylib" ${FRAMEWORKS} -current_version ${VERSION} -read_only_relocs suppress `cat ${TMP_DIR}/link.res` -framework Cocoa
    if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
    
    CleanTmp
}

# 
# Create fat dylib
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${OUT_DIR}/libSGSDK${1}.dylib" -arch ${2} "${OUT_DIR}/libSGSDK${2}.dylib" -output "${OUT_DIR}/libSGSDK.dylib" -create
    
    rm -rf "${OUT_DIR}/libSGSDK${1}.dylib"
    rm -rf "${OUT_DIR}/libSGSDK${2}.dylib"
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
        ADD_SYMLINKS="Y"
    else
        rm -rf "${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
        ADD_SYMLINKS="N"
    fi
    
    mkdir ${VERSION_DIR}
    mkdir ${HEADER_DIR}
    mkdir ${RESOURCES_DIR}
    
    cp "${SDK_SRC_DIR}/SGSDK.h" "${HEADER_DIR}/SGSDK.h"
    cp "${SDK_SRC_DIR}/sgTypes.h" "${HEADER_DIR}/sgTypes.h"
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
    #link is relative to Current link...
    if [ -d ./Versions/Current ]
    then
        rm ./Versions/Current
    fi
    ln -f -s "./${VERSION}" "./Versions/Current"
    
    if [ $ADD_SYMLINKS = "Y" ]
    then
        ln -f -s "./Versions/Current/Resources" "./Resources"
        ln -f -s "./Versions/Current/Headers" "./Headers"
        ln -f -s "./Versions/Current/SGSDK" "./SGSDK"
    fi
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
        echo "          Creating SwinGame Dynamic Library"
        echo "                 for Mac OS X"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Copying Frameworks from ${FRAMEWORK_DIR}"
        echo "  Compiling with $EXTRA_OPTS"
        echo "--------------------------------------------------"
        
        echo "  ... Compiling Library"
        FPC_BIN=`which ppcppc`
        doMacCompile "ppc"
        
        FPC_BIN=`which ppc386`
        doMacCompile "i386"
        
        doLipo "i386" "ppc"
        
        doCreateFramework
        
        if [ ! $INSTALL = "N" ]
        then
            echo "  ... Installing SwinGame"
            if [ $INSTALL = "local" ]
            then
                TO_DIR=~/Library/Frameworks
            elif  [ $INSTALL = "global" ]
            then
                TO_DIR=/Library/Frameworks
            else
                echo "Unknown install type"
                exit -1
            fi
            
            if [ ! -d ${TO_DIR} ]
            then
                mkdir -p ${TO_DIR}
            fi
            
            doCopyFramework()
            {
                # $1 = framework
                # $2 = dest
                fwk_name=${1##*/} # ## = delete longest match for */... ie all but file name
                
                if [ -d ${2}/${fwk_name} ]
                then
                    #framework exists at destn, just copy the version details
                    rm $2/${fwk_name}/Versions/Current
                    cp -p -R -f ${1}/Versions/* ${2}/${fwk_name}/Versions
                else
                    cp -p -R $1 $2
                fi
            }
            
            doCopyFramework "${OUT_DIR}"/SGSDK.framework "${TO_DIR}"
            for file in `find ${FRAMEWORK_DIR} -depth 1 | grep [.]framework$`
            do
                doCopyFramework ${file} "${TO_DIR}"
            done
        fi
    else
        echo "--------------------------------------------------"
        echo "          Creating SwinGame Dynamic Library"
        echo "                 for Linux"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Compiling with $EXTRA_OPTS"
        echo "--------------------------------------------------"
        
        if [ $? != 0 ]; then echo "Error creating pascal library SGSDK"; cat ${LOG_FILE}; exit 1; fi
        
        echo "  ... Compiling Library"
        fpc -Mdelphi $EXTRA_OPTS -FE"${OUT_DIR}" ${SDK_SRC_DIR}/sgsdk.pas >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${LOG_FILE}; exit 1; fi
        
        mv "${OUT_DIR}/libSGSDK.so" "${OUT_DIR}/libSGSDK.so.${VERSION}"
        
        if [ ! $INSTALL = "N" ]
        then
            echo "  ... Installing SwinGame"
            mv -f "${OUT_DIR}/libSGSDK.so.${VERSION}" /usr/lib
            ln -s -f "/usr/lib/libSGSDK.so.${VERSION}" /usr/lib/libSGSDK.so
        
            ldconfig -v - n
        fi
        
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