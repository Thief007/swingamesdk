#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH
APP_PATH="."

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
FULL_OUT_DIR="${FULL_APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"
LOG_FILE="${APP_PATH}/out.log"

if [ "$OS" = "$MAC" ]; then
    ICON="SwinGame.icns"
else
    ICON="SwinGame"
fi

#
# Library versions
#
OPENGL=false
SDL_13=false

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

while getopts chb:g: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    b)  if [ "${OPTARG}" = "adass" ]; then
            SDL_13=true
        fi 
        ;;
    h)  Usage ;;
    g)  if [ "${OPTARG}" = "odly" ]; then
            OPENGL=true
        fi 
        ;;
    esac
done

shift $((${OPTIND}-1))

if [ "$OS" = "$MAC" ]; then
    if [ ${SDL_13} = true ]; then
      TMP_DIR="${APP_PATH}/tmp/sdl13"
      LIB_DIR="${APP_PATH}/staticlib/sdl13/mac"
    elif [ ${OPENGL} = true ]; then
      TMP_DIR="${APP_PATH}/tmp/godly"
      LIB_DIR="${APP_PATH}/staticlib/godly/mac"
    else
      TMP_DIR="${APP_PATH}/tmp/sdl12"
      LIB_DIR="${APP_PATH}/staticlib/sdl12/mac"
    fi
elif [ "$OS" = "$WIN" ]; then
    if [ ${SDL_13} = true ]; then
      LIB_DIR="${APP_PATH}/lib/sdl13/win"
    elif [ ${OPENGL} = true ]; then
      LIB_DIR="${APP_PATH}/lib/sdl13/win"
    else
      LIB_DIR="${APP_PATH}/lib/win"
    fi
fi


#
# Set compiler path and options
#
FPC_BIN=`which fpc`
if [ "$OS" = "$WIN" ]; then
    PAS_FLAGS="-g -Ci -gc -Ct -dTrace"
else
    PAS_FLAGS="-g -Ci -gw -Ct -dTrace"
fi    


if [ ${SDL_13} = true ]; then
  PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL13"
elif [ ${OPENGL} = true ]; then
  PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_OPENGL -dSWINGAME_SDL13"
else
  PAS_FLAGS="${PAS_FLAGS} -k\"-lstdc++\""
fi

echo ${PAS_FLAGS}

DRV_LIB=`find ./libsrc -type d ! -path \*.svn\* | awk -F . '{print "-Fu"$0}'`
SG_INC="-Fi${APP_PATH}/libsrc -Fu${APP_PATH}/libsrc -Fu${APP_PATH}/src ${DRV_LIB}"
CLEAN="N"

#
# Create SwinGame.pas if missing
#
if [[ ! -f ./test/SwinGame.pas ]]; then
  echo " Creating SwinGame.pas"
  cd ../Tools/SGWrapperGen
  python create_pascal_library.py
  cd "${FULL_APP_PATH}"
  cp ../Generated/Pascal/lib/SwinGame.pas ./test/SwinGame.pas
fi


#
# Set game name
#
GAME_NAME="Test"

#
# Locate GameMain.pas
#
locateGameMain()
{
  cd "${FULL_APP_PATH}/test"
  fileList=$(find "." -maxdepth 1 -type f -name \*.pas)
  FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)
  
  if [[ ${FILE_COUNT} = 1 ]]; then
    GAME_MAIN=${fileList[0]}
  else
    echo "Select the file to compile for your game"
    PS3="File number: "
  
    select fileName in $fileList; do
        if [ -n "$fileName" ]; then
            GAME_MAIN=${fileName}
        fi
      
        break
    done
  fi
  
  cd ${FULL_APP_PATH}
}

#
# Get the name of the source file
#
locateGameMain
SRC_FILE=$GAME_MAIN

if [ ! -f ./test/$SRC_FILE ]; then
    echo "Unable to locate source file"
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
    if [ -d "${APP_PATH}/tmp" ]
    then
        rm -rf "${APP_PATH}/tmp"
    fi
    mkdir -p "${TMP_DIR}"
    
}

DoDriverMessage()
{
  if [ ${SDL_13} = true ]; then
    echo "  ... Using SDL 1.3 Driver"
  elif [ ${OPENGL} = true ]; then
    echo "  ... Using OpenGL Driver"
  else
    echo "  ... Using SDL 1.2 Driver"
  fi
}

doBasicMacCompile()
{
    mkdir -p ${TMP_DIR}/${1}
    
    echo "  ... Compiling $GAME_NAME - $1"
    
    # FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($2,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    
    FRAMEWORKS='-framework AudioToolbox -framework AudioUnit -framework CoreAudio -framework IOKit -framework OpenGL -framework QuickTime -framework Carbon -framework ForceFeedback'

    STATIC_LIBS=`cd ${LIB_DIR};ls -f *.a | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-l%s ", substr(patharr[idx],4)) }'`
    
    
    ${FPC_BIN} ${PAS_FLAGS} ${SG_INC} -Mobjfpc -gl -gw2 -Sew -Sh -FE"${TMP_DIR}/${1}" -FU"${TMP_DIR}/${1}" -Fu"${LIB_DIR}" -Fi"${SRC_DIR}" -k"${STATIC_LIBS}" -k"-rpath @loader_path/../Frameworks" -k"-F${LIB_DIR} -framework Cocoa ${FRAMEWORKS}" -k"-lbz2" $2 -o"${OUT_DIR}/${GAME_NAME}" "./test/${SRC_FILE}" > ${LOG_FILE} 2> ${LOG_FILE}
    
    if [ $? != 0 ]; then DoExitCompile; fi
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doMacCompile()
{
    mkdir -p ${TMP_DIR}/${1}
    echo "  ... Compiling $GAME_NAME - $1"
    
    FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($2,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    
    ${FPC_BIN} ${PAS_FLAGS} ${SG_INC} -Mobjfpc -gl -gw2 -Sew -Sh -FE"${TMP_DIR}/${1}" -FU"${TMP_DIR}/${1}" -Fu"${LIB_DIR}" -Fi"${SRC_DIR}" -k"-F${LIB_DIR} -framework Cocoa ${FRAMEWORKS}" $2 -o"${OUT_DIR}/${GAME_NAME}.${1}" "./test/${SRC_FILE}" > ${LOG_FILE} 2> ${LOG_FILE}
    
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
    GAMEAPP_PATH="${FULL_OUT_DIR}/${GAME_NAME}.app"
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

    # echo "  ... Added Private Frameworks"
    # cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"

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
    
    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -gh -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${SRC_DIR} -o${GAME_NAME} ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doLinuxPackage()
{
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
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
    
    #echo "  ... Creating Resources"
    #windres ${SRC_DIR}/SwinGame.rc ${SRC_DIR}/GameLauncher.res
    #if [ $? != 0 ]; then DoExitCompile; fi
    
    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${LIB_DIR} -o${GAME_NAME}.exe ./test/${SRC_FILE} > ${LOG_FILE}
    #${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${TMP_DIR} -Fi${LIB_DIR} -FU${TMP_DIR} -s ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doWindowsPackage()
{
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
    
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
    
    copyWithoutSVN "${FULL_APP_PATH}/test/Resources" "${RESOURCE_DIR}"
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
    echo "  Running script from $FULL_APP_PATH"
    echo "  Saving output to $OUT_DIR"
    echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
    echo "--------------------------------------------------"
    echo "  ... Creating ${GAME_NAME}"    
    DoDriverMessage;
    if [ "$OS" = "$MAC" ]; then
        HAS_PPC=false
        HAS_i386=false
        HAS_LEOPARD_SDK=false
        HAS_LION=false
        OS_VER=`sw_vers -productVersion | awk -F . '{print $1"."$2}'`
        
        if [ -f /usr/libexec/as/ppc/as ]; then
            HAS_PPC=true
        fi
        
        if [ -f /usr/libexec/as/i386/as ]; then
            HAS_i386=true
        fi
        
        if [ -d /Developer/SDKs/MacOSX10.5.sdk ]; then
            HAS_LEOPARD_SDK=true
        fi
        
        if [ $OS_VER = '10.5' ]; then
            HAS_LEOPARD_SDK=true
        fi
        
        if [ $OS_VER = '10.7' ]; then
            HAS_LION=true
        fi

        if [ $OS_VER = '10.8' ]; then
            HAS_LION=true
        fi
        
        if [[ $HAS_i386 = true && $HAS_PPC = true && $HAS_LEOPARD_SDK = true ]]; then
            echo "  ... Building Universal Binary"
            
            if [ $OS_VER = '10.5' ]; then
                FPC_BIN=`which ppc386`
                doMacCompile "i386" ""
                
                FPC_BIN=`which ppcppc`
                doMacCompile "ppc" ""
            else
                FPC_BIN=`which ppc386`
                doMacCompile "i386" "-k-syslibroot -k/Developer/SDKs/MacOSX10.5.sdk -k-macosx_version_min -k10.5"
                
                FPC_BIN=`which ppcppc`
                doMacCompile "ppc" "-k-syslibroot -k/Developer/SDKs/MacOSX10.5.sdk -k-macosx_version_min -k10.5"
            fi
            
            doLipo "i386" "ppc"
        else
            if $HAS_LION ; then
                PAS_FLAGS="$PAS_FLAGS -k-macosx_version_min -k10.7 -k-no_pie"
            fi
            doBasicMacCompile
        fi
        
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
cd "${FULL_APP_PATH}"
if [ "$OS" = "$MAC" ]; then
    ${APP_PATH}/bin/Test.app/Contents/MacOS/Test
else
    ${APP_PATH}/bin/Test
fi