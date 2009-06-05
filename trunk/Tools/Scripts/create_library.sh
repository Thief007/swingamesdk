#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
SWINGAME_PATH=`cd ../..; pwd`
OUT_DIR="${SWINGAME_PATH}/bin/lib"
TMP_DIR="${SWINGAME_PATH}/tmp"
LIBS=
SDK_SRC_DIR=`cd ${SWINGAME_PATH}/CoreSDK/src; pwd`

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

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doCompile()
{
    CleanTmp
    
    ${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"${TMP_DIR}" -FU"${TMP_DIR}" -s ${SDK_SRC_DIR}/sgsdk1.pas >> ${APP_PATH}/out.log
    
    if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${APP_PATH}/out.log; exit 1; fi

    #Assemble all of the .s files
    echo "  ... Assembling library for $1"
    
    for file in `find ${TMP_DIR} | grep [.]s$`
    do
        /usr/bin/as -o ${file%.s}.o $file -arch $1
        if [ $? != 0 ]; then DoExitAsm $file; fi
        rm $file
    done

    echo "  ... Linking Library"
    /usr/bin/libtool -install_name ./libSGSDK.dylib  -arch_only ${1} -dynamic -L"${TMP_DIR}" -search_paths_first -multiply_defined suppress -o "${TMP_DIR}/libSGSDK${1}.dylib" `cat ${SDK_SRC_DIR}/maclink${1}.res` `ls ${TMP_DIR}/*.o` -current_version 2.0
    if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
        
    #rm "${Output}"/*.o 2>> /dev/null
    rm "${Output}"/*.s 2>> /dev/null
    rm "${Output}"/*.ppu 2>> /dev/null
    rm "${Output}"/link.res 2>> /dev/null
    rm "${Output}"/ppas.sh 2>> /dev/null
}

# 
# Create fat dylib
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "$Output/libSGSDK${1}.dylib" -arch ${2} "$Output/libSGSDK${2}.dylib" -output "$Output/libSGSDK.dylib" -create

    rm -rf "$Output/libSGSDK${1}.dylib"
    rm -rf "$Output/libSGSDK${2}.dylib"
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

if [ $CLEAN = "N" ]
then
    if [ -f ${BaseDir}/out.log ]
    then
        rm -f ${BaseDir}/out.log
    fi

    if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
    then
        Libs=${BaseDir}/lib/mac
        
        echo "__________________________________________________"
        echo "Building Mac version"
        echo "__________________________________________________"
        echo "  Running script from $BaseDir"
        echo "  Saving output to $Output"
        echo "  Copying Frameworks from $Libs"
        echo "  Compiling with $EXTRA_OPTS"
        echo "__________________________________________________"

        echo "  ... Compiling Library"

        FPC_BIN=`which ppcppc`
        doCompile "ppc"
        
        FPC_BIN=`which ppc386`
        doCompile "i386"
        
        doLipo "i386" "ppc"
    
        cd $DOTNETlocn
        
        echo "  ... Compiling .NET Library"
        xbuild $DOTNETproj >> ${BaseDir}/out.log
        if [ $? != 0 ]; then echo "Error with xbuild"; cat ${BaseDir}/out.log; exit 1; fi
            
        echo "  ... Copying Library to $Output"
        cp "$DOTNETbin"/*.dll "$Output"
        cp "$DOTNETbin"/*.XML "$Output"
        rm "$DOTNETbin"/*
    else
        echo "__________________________________________________"
        echo "Building Linux version"
        echo "__________________________________________________"
        echo "  Running script from $BaseDir"
        echo "  Saving output to $Output"
        echo "__________________________________________________"
        
        fpc -v0 -g -Mdelphi $EXTRA_OPTS -FE"$Output" ./src/SGSDK.pas >> out.log
        if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${BaseDir}/out.log; exit 1; fi

        rm "$Output"/*.o
        rm "$Output"/*.ppu

        cd $DOTNETlocn
        xbuild $DOTNETproj >> ${BaseDir}/out.log
        if [ $? != 0 ]; then echo "Error with xbuild"; cat ${BaseDir}/out.log; exit 1; fi
        cp "$DOTNETbin"/*.dll "$Output" 
        cp "$DOTNETbin"/*.XML "$Output" 
    fi
else
    rm -rf "$Output"
    mkdir "$Output"
    echo    ... Cleaned
fi

cpToSDK "C#"
cpToSDK VB

echo "  Finished"
echo "__________________________________________________"