#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
SWINGAME_PATH=`cd ../..; pwd`
OUT_DIR="${SWINGAME_PATH}/bin/c_wrapper"
TMP_DIR="${SWINGAME_PATH}/tmp"
SDK_SRC_DIR="${SWINGAME_PATH}/CoreSDK/src"
FRAMEWORK_DIR="${SWINGAME_PATH}/CoreSDK/lib/mac"
LOG_FILE="${APP_PATH}/out.log"

PYTHON_SCRIPT_DIR=${SWINGAME_PATH}/Tools/SGWrapperGen
C_LIB_FILES_DIR=${PYTHON_SCRIPT_DIR}/out
LIBARY_DIR=${SWINGAME_PATH}/bin/lib/

GCC_BIN=`which gcc`
LIBTOOL_BIN=`which libtool`

CLEAN="N"

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
    else
        mkdir "${TMP_DIR}" 
    fi
}

CreateCWrapper()
{
    cd ${PYTHON_SCRIPT_DIR}
    
    if [ -d "${C_LIB_FILES_DIR}" ]
    then
        rm "${C_LIB_FILES_DIR}/*.c" 2>> /dev/null
        rm "${C_LIB_FILES_DIR}/*.h" 2>> /dev/null
    fi
    python create_c_library.py
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doCompile()
{
    CleanTmp
    
    echo "  ... Compiling for $1"
    for file in `find ${C_LIB_FILES_DIR} | grep [.]c$`
    do
        name=${file##*/}
        name=${name%%.c}
        echo "      ... Compiling ${name}"    
        ${GCC_BIN} -c -g -arch ${1} -o "${TMP_DIR}/${name}.o" ${file} >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling SwinGame C wrapper"; cat ${LOG_FILE}; exit 1; fi
    done
    
    #Assemble all of the .s files
    echo "  ... Creating archive for $1"
    
    ${LIBTOOL_BIN} -static -o ${TMP_DIR}/libSwinGame_${1}.a -arch_only ${1} ${TMP_DIR}/*.o >> ${LOG_FILE}
    if [ $? != 0 ]; then echo "Error creating archive"; cat ${LOG_FILE}; exit 1; fi
    
    CleanTmp
}

# 
# Create fat dylib
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${TMP_DIR}/libSwinGame_${1}.a" -arch ${2} "${TMP_DIR}/libSwinGame_${2}.a" -output "${OUT_DIR}/libSwinGame.a" -create
    
    rm -rf "${TMP_DIR}/libSwinGame${1}.a"
    rm -rf "${TMP_DIR}/libSwinGame${2}.a"
}

while getopts ch o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
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
        echo "__________________________________________________"
        echo "Building Mac version"
        echo "__________________________________________________"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "__________________________________________________"
        
        echo "  ... Creating C wrapper library"
        CreateCWrapper >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error creating c wrapper library"; cat ${LOG_FILE}; exit 1; fi
            
        echo "  ... Compiling Library"
        doCompile "ppc"
        doCompile "i386"
        
        doLipo "i386" "ppc"
    else
        echo "__________________________________________________"
        echo "Building Linux version"
        echo "__________________________________________________"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Compiling with $EXTRA_OPTS"
        echo "__________________________________________________"
        
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
echo "__________________________________________________"