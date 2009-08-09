#!/bin/bash

#
# Step 1: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

#
# Step 2: Detect the operating system (defines MAC, LIN, WIN, and OS variables)
#
source "${APP_PATH}/inc/os_check.sh"

#
# Step 3: Set the paths to local variables
#
PYTHON_SCRIPT='create_csharp_library.py'
source "${APP_PATH}/inc/base_template_dirs.sh"

#
# Step 4: Set up array of files to copy
#
if [ "$OS" = "$WIN" ]; then
    COPY_LIST=( "VS08,${VS08_TEMPLATE_DIR},${VS08_DIST_DIR}" )
    #COPY_LIST=( "${COPY_LIST[@]}" "Command Line,${CL_TEMPLATE_DIR},${CL_DIST_DIR}")
    
    #Simple copy list with VS templates
    SMPL_COPY_LIST=( "C# Express 08,${STUDIO_EX_CS_08_TEMP_DIR},${STUDIO_EX_CS_08_DIST_DIR}" )
else
    COPY_LIST=( "Mono,${MONO_TEMPLATE_DIR},${MONO_DIST_DIR}" )
fi

#
# Step 5: Declare functions
#
source ${APP_PATH}/inc/copy_without_svn.sh
source ${APP_PATH}/inc/dist_dir.sh

CreateCSCode()
{
    cd ${PYTHON_SCRIPT_DIR}
    python ${PYTHON_SCRIPT}
}

#
# Step 6: Create c# library and copy
#
echo "--------------------------------------------------"
echo "      Creating $OS SwinGame C# Templates"
echo "--------------------------------------------------"
echo "  Will Create Templates for: "
ListDists "${COPY_LIST}"
echo "--------------------------------------------------"
echo "  Python scripts at $PYTHON_SCRIPT_DIR"

if [ "$OS" = "$MAC" ]; then
    echo "  Copying Frameworks from Source dist"
elif [ "$OS" = "$WIN" ]; then
    echo "  Copying libraries from Source dist"
fi

echo "--------------------------------------------------"
echo "  ... Creating C# library code"
CreateCSCode

DoDist "${COPY_LIST}" "${CS_DIST_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_CS_TEMPLATE_DIR}"

if [ "$OS" = "$WIN" ]; then
    echo "  ... Creating Visual Studio Template Structure"
    rm -rf "${STUDIO_DIST_DIR}"
    DoCopy "${SMPL_COPY_LIST}"

    #Go to the VS08 C# template dir
    echo "  ... Creating Project Template for C# Express"
    cd "${VS08_DIST_DIR}"
    rm *.sln
    #cat "src/GameMain.cs" | awk '{sub("MyGame", "[!output SAFE_NAMESPACE_NAME].src"); print}' >> "src/NewGameMain.cs"
    cat "src/GameMain.cs" | awk '{sub("MyGame", "$safeprojectname$.src"); print}' >> "src/NewGameMain.cs"
    mv "src/NewGameMain.cs" "src/GameMain.cs"
    zip -r "SwinGame C# Project.zip" * > /dev/null

    echo "  ... Creating Template Installer for C# Express"
    mv "SwinGame C# Project.zip" "${STUDIO_EX_CS_08_DIST_DIR}"
    cd "${STUDIO_EX_CS_08_DIST_DIR}"
    zip -r "SwinGame C# Template Installer.vsi" .vscontent * > /dev/null
fi

echo "  Finished"
echo "--------------------------------------------------"
