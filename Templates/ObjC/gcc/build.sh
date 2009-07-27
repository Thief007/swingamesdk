#!/bin/sh

#
# Step 1: Detect the operating system
#
MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]; then
    OS=$MAC
elif [ -d /c/Windows ]; then
    OS=$WIN
else
    OS=$LIN
fi

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

C_FLAGS="-O3 -Wall"
SG_INC="-I${APP_PATH}/lib/"

GCC_BIN=`which gcc`

GAME_NAME=${APP_PATH##*/}
ICON=SwinGame

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
    echo " -h   Show this help message"
    echo " -d   Create a debug build"
    echo " -i [icon] Change the icon file"
    exit 0
}

while getopts chdi: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    d)  DEBUG="Y";;
    i)  ICON="$OPTARG";;
    ?)  Usage
    esac
done

shift $((${OPTIND}-1))

if [ "a$1a" != "aa" ]; then
    GAME_NAME=$1
fi

#
# Change directories based on release or debug builds
#
if [ "a${DEBUG}a" != "aa" ]; then
    C_FLAGS="-g -Wall"
    OUT_DIR="${OUT_DIR}/Debug"
    TMP_DIR="${TMP_DIR}/Debug"
else
    OUT_DIR="${OUT_DIR}/Release"
    TMP_DIR="${TMP_DIR}/Release"
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

#
# Compile the passed in file
# $1 = filename
# $2 = name
# $3 = out filename
# $4 = extra options
#
doCompile()
{
    file=$1
    name=$2
    out_file=$3
    extra_opts=$4
    
    if [ $file -nt $out_file ]; then
        echo "      ... Compiling ${name}"    
        ${GCC_BIN} -c ${extra_opts} ${SG_INC} ${C_FLAGS} -o "${out_file}" ${file} >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling"; cat ${LOG_FILE}; exit 1; fi
    fi
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doMacCompile()
{
    mkdir -p "${TMP_DIR}/${1}"
    
    echo "  ... Compiling for $1"
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]c$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.c} # %% = delete longest match from back, i.e. extract .c
        
        out_file="${TMP_DIR}/${1}/${name}.o"
        
        doCompile "${file}" "${name}" "${out_file}" "-arch ${1}"
    done
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]m$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.m} # %% = delete longest match from back, i.e. extract .c
        
        out_file="${TMP_DIR}/${1}/${name}.o"
        
        doCompile "${file}" "${name}" "${out_file}" "-arch ${1}"
    done
    
    #Assemble all of the .o files
    echo "  ... Creating game for $1"
    FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    
    ${GCC_BIN} -F${LIB_DIR} ${FRAMEWORKS} -framework Foundation -framework Cocoa -arch $1 -o ${TMP_DIR}/${1}/${GAME_NAME} `find ${TMP_DIR}/${1} -maxdepth 1 -name \*.o`
    if [ $? != 0 ]; then echo "Error creating game"; cat ${LOG_FILE}; exit 1; fi
}

# 
# Create fat executable (i386 + ppc)
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${TMP_DIR}/${1}/${GAME_NAME}" -arch ${2} "${TMP_DIR}/${2}/${GAME_NAME}" -output "${OUT_DIR}/${GAME_NAME}" -create
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

    mv "${OUT_DIR}/${GAME_NAME}" "${GAMEAPP_PATH}/Contents/MacOS/" 

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
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]c$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.c} # %% = delete longest match from back, i.e. extract .c
        doCompile "${file}" "${name}" "${TMP_DIR}/${name}.o" ""
    done
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]m$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.m} # %% = delete longest match from back, i.e. extract .c
        doCompile "${file}" "${name}" "${TMP_DIR}/${name}.o" ""
    done
    
    #Assemble all of the .s files
    echo "  ... Creating game"
    
    ${GCC_BIN} -L${LIB_DIR} -lsgsdk -o ${OUT_DIR}/${GAME_NAME} `find ${TMP_DIR} -maxdepth 1 -name \*.o`
    if [ $? != 0 ]; then echo "Error creating game"; cat ${LOG_FILE}; exit 1; fi
}

doLinuxPackage()
{
    RESOURCE_DIR="${APP_PATH}/bin/Resources"
}

doWindowsCompile()
{
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]c$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.c} # %% = delete longest match from back, i.e. extract .c
        doCompile "${file}" "${name}" "${TMP_DIR}/${name}.o" ""
    done
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]m$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.m} # %% = delete longest match from back, i.e. extract .c
        doCompile "${file}" "${name}" "${TMP_DIR}/${name}.o" ""
    done
    
    #Assemble all of the .s files
    echo "  ... Creating game"
    
    ${GCC_BIN} -L${LIB_DIR} -lsgsdk -o ${OUT_DIR}/${GAME_NAME} `find ${TMP_DIR} -maxdepth 1 -name \*.o`
    if [ $? != 0 ]; then echo "Error creating game"; cat ${LOG_FILE}; exit 1; fi
}

doWindowsPackage()
{
    RESOURCE_DIR=${APP_PATH}/bin/Resources
    
    echo "  ... Copying libraries"
    cp -p -f "${LIB_DIR}"/*.dll "${OUT_DIR}"
    cp -p -f "${LIB_DIR}"/*.a "${OUT_DIR}"
}

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 -type d ! -path \*.svn\* -exec sh -c "if [ ! -d '${TO_DIR}/{}' ]; then mkdir -p '${TO_DIR}/{}' ; fi" \;
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
        doMacCompile "ppc"
        doMacCompile "i386"
        
        doLipo "i386" "ppc"
        doMacPackage
    elif [ "$OS" = "$LIN" ]; then
        doLinuxCompile
        doLinuxPackage
    else #Windows
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

echo "  Finished"
echo "--------------------------------------------------"