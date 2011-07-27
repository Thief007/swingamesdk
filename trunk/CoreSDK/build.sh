#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Step 1: Detect the operating system
#
MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ `uname` = "Darwin" ]; then
    OS=$MAC
elif [ `uname` = "Linux" ]; then
    OS=$LIN
else
    OS=$WIN
fi

#
# Set the basic paths
#
OUT_DIR="${APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"
LOG_FILE="${APP_PATH}/out.log"

if [ "$OS" = "$MAC" ]; then
    LIB_DIR="${APP_PATH}/lib/mac"
elif [ "$OS" = "$WIN" ]; then
    LIB_DIR="${APP_PATH}/lib/win"
fi

#
# Set compiler path and options
#
FPC_BIN=`which fpc`
if [ "$OS" = "$WIN" ]; then
    PAS_FLAGS="-g -Cr -Ci -gc -Ct"
else
    PAS_FLAGS="-gw -Cr -Ci -Ct"
fi
SG_INC="-Fi${APP_PATH}/libsrc -Fu${APP_PATH}/libsrc -Fu${APP_PATH}/src"
CLEAN="N"


#
# Set game name
#
GAME_NAME="Test"


Usage()
{
    echo "Usage: [-c] [-h] src_name"
    echo 
    echo "Compiles your game into an executable application."
    echo "Output is located in $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -h   Show this help message "
    exit 0
}

while getopts chd o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    esac
done

shift $((${OPTIND}-1))

#
# Get the name of the source file
#
SRC_FILE=${1}.pas

if [ ! -f ./test/$SRC_FILE ]; then
    echo "usage build.sh filename"
    exit 1
fi

#
# Remove old log file
#
if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


DoExitCompile ()
{ 
    echo "An error occurred while compiling"; 
    cat out.log
    exit 1; 
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
    mkdir -p ${TMP_DIR}/${1}
    echo "  ... Compiling $GAME_NAME - $1"
    
    FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    
    ${FPC_BIN} ${PAS_FLAGS} ${SG_INC} -Mobjfpc -gh -gl -gw2 -Sew -Sh -FE"${TMP_DIR}/${1}" -FU"${TMP_DIR}/${1}" -Fu"${LIB_DIR}" -Fi"${SRC_DIR}" -k"-F${LIB_DIR} -framework Cocoa ${FRAMEWORKS}" $2 -o"${OUT_DIR}/${GAME_NAME}.${1}" "./test/${SRC_FILE}" > ${LOG_FILE} 2> ${LOG_FILE}
    
    #-CioOR
    
    if [ $? != 0 ]; then DoExitCompile; fi
    
    # mkdir -p ${TMP_DIR}/$1
    # echo "  ... Compiling $GAME_NAME - $1 (${SRC_FILE})"
    # 
    # ${FPC_BIN}  $PAS_FLAGS ${SG_INC} -Mobjfpc -Sh -FE${TMP_DIR}/$1 -Fi${LIB_DIR} -FU${TMP_DIR}/$1 -s ./test/${SRC_FILE} > ${LOG_FILE}
    # if [ $? != 0 ]; then DoExitCompile; fi
    # rm -f ${LOG_FILE}
    # 
    # #Remove the pascal assembler script
    # rm ${TMP_DIR}/$1/ppas.sh
    # 
    # echo "  ... Assembling for $1"
    # 
    # #Assemble all of the .s files
    # for file in `find ${TMP_DIR}/$1 | grep [.]s$`
    # do
    #     /usr/bin/as -o ${file%.s}.o $file -arch $1
    #     if [ $? != 0 ]; then DoExitAsm $file; fi
    #     rm $file
    # done
    # 
    # echo "  ... Linking ${GAME_NAME}"
    # 
    # FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    # 
    # /usr/bin/ld /usr/lib/crt1.o -F${LIB_DIR} -L/usr/X11R6/lib -L/usr/lib -search_paths_first -multiply_defined suppress -o "${OUT_DIR}/${GAME_NAME}.${1}" `cat ${TMP_DIR}/$1/link.res` -framework Cocoa ${FRAMEWORKS}
    # if [ $? != 0 ]; then DoExitCompile ${GAME_NAME}; fi
}

# 
# Create fat executable (i386 + ppc)
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${OUT_DIR}/${GAME_NAME}.${1}" -arch ${2} "${OUT_DIR}/${GAME_NAME}.${2}" -output "${OUT_DIR}/${GAME_NAME}" -create
    
    rm -f "${OUT_DIR}/${GAME_NAME}.${1}"
    rm -f "${OUT_DIR}/${GAME_NAME}.${2}"
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
    
    mkdir "${GAMEAPP_PATH}"
    mkdir "${GAMEAPP_PATH}/Contents"
    mkdir "${GAMEAPP_PATH}/Contents/MacOS"
    mkdir "${GAMEAPP_PATH}/Contents/Resources"
    mkdir "${GAMEAPP_PATH}/Contents/Frameworks"

    echo "  ... Added Private Frameworks"
    cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"

    mv "${OUT_DIR}/${GAME_NAME}" "${APP_PATH}/bin/${GAME_NAME}.app/Contents/MacOS/" 

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

doLinuxCompile()
{
    mkdir -p ${TMP_DIR}
    echo "  ... Compiling $GAME_NAME"
    
    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${SRC_DIR} -o${GAME_NAME} ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doLinuxPackage()
{
    RESOURCE_DIR="${OUT_DIR}/Resources"
}

doWindowsCompile()
{
    mkdir -p ${TMP_DIR}
    
    echo "  ... Compiling $GAME_NAME"
    
    LIB_DIR=`echo $LIB_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    TMP_DIR=`echo $TMP_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    SRC_DIR=`echo $SRC_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    OUT_DIR=`echo $OUT_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    
    echo "  ... Creating Resources"
    #windres ${SRC_DIR}/SwinGame.rc ${SRC_DIR}/GameLauncher.res
    if [ $? != 0 ]; then DoExitCompile; fi
    
    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${LIB_DIR} -o${GAME_NAME}.exe ./test/${SRC_FILE} > ${LOG_FILE}
    #${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${TMP_DIR} -Fi${LIB_DIR} -FU${TMP_DIR} -s ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doWindowsPackage()
{
    RESOURCE_DIR=${OUT_DIR}/Resources
    
    echo "  ... Copying libraries"
    cp -p -f "${LIB_DIR}"/*.dll "${OUT_DIR}"
}

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -exec mkdir -p "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

#
# Copy Resources from standard location to $RESOURCE_DIR
#
doCopyResources()
{
    echo "  ... Copying Resources into $GAME_NAME"
    
    copyWithoutSVN "${APP_PATH}/test/Resources" "${RESOURCE_DIR}"
}


if [ $CLEAN = "N" ]
then
    CleanTmp
    if [ ! -d "${OUT_DIR}" ]
    then
        mkdir -p "${OUT_DIR}"
    fi
    echo "--------------------------------------------------"
    echo "          Creating $GAME_NAME"
    echo "          for $OS"
    echo "--------------------------------------------------"
    echo "  Running script from $APP_PATH"
    echo "  Saving output to $OUT_DIR"
    echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
    echo "--------------------------------------------------"
    echo "  ... Creating ${GAME_NAME}"    
    
    if [ "$OS" = "$MAC" ]; then
        FPC_BIN=`which ppc386`
        doMacCompile "i386" "-k-syslibroot -k/Developer/SDKs/MacOSX10.5.sdk -k-macosx_version_min -k10.5"
        FPC_BIN=`which ppcppc`
        doMacCompile "ppc" "-k-syslibroot -k/Developer/SDKs/MacOSX10.5.sdk -k-macosx_version_min -k10.5"
        
        doLipo "i386" "ppc"
        doMacPackage
    elif [ "$OS" = "$LIN" ]; then
        doLinuxCompile
        doLinuxPackage
    else
        doWindowsCompile
        doWindowsPackage
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
#rm -rf ${TMP_DIR} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"
if [ "$OS" = "$MAC" ]; then
    ${APP_PATH}/bin/Test.app/Contents/MacOS/Test
else
    ${APP_PATH}/bin/Test
fi