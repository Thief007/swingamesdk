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

# Remove all previously generated files
find ${GENERATED_DIR} -type f ! -path \*.svn\* -exec rm -f {} \;

#
# Step 4: Set up array of files to copy
#
if [ "$OS" = "$WIN" ]; then
    COPY_LIST=( "VS08,${VS08_TEMPLATE_DIR},${VS08_DIST_DIR}" )
    #COPY_LIST=( "${COPY_LIST[@]}" "Command Line,${CL_TEMPLATE_DIR},${CL_DIST_DIR}")
    
    VB_COPY_LIST=( "VS08,${VB_VS08_TEMPLATE_DIR},${VB_VS08_DIST_DIR}" )
else
    COPY_LIST=( "Mono C#,${MONO_TEMPLATE_DIR},${MONO_DIST_DIR}" )
    
    VB_COPY_LIST=( "Mono VB,${VB_MONO_TEMPLATE_DIR},${VB_MONO_DIST_DIR}" )
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
echo "      Creating $OS SwinGame C# & VB Templates"
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

#
# Step 7: Compile library
#
echo "  ... Compiling the C# library code"
if [ "$OS" = "$WIN" ]; then
    CS_LIBRARY_DIR=`echo $CS_LIBRARY_DIR | awk '{sub("/c/", "c:\\\\"); print}'`
    CS_LIBRARY_DIR=`echo $CS_LIBRARY_DIR | awk '{gsub("/", "\\\\"); print}'`
    
    CS_GENERATED_LIB_DIR=`echo $CS_GENERATED_LIB_DIR | awk '{sub("/c/", "c:\\\\"); print}'`
    CS_GENERATED_LIB_DIR=`echo $CS_GENERATED_LIB_DIR | awk '{gsub("/", "\\\\"); print}'`
    
    CS_GENERATED_CODE_DIR=`echo $CS_GENERATED_CODE_DIR | awk '{sub("/c/", "c:\\\\"); print}'`
    CS_GENERATED_CODE_DIR=`echo $CS_GENERATED_CODE_DIR | awk '{gsub("/", "\\\\"); print}'`
    
    csc -t:library -r:System.dll -r:System.Drawing.dll -define:DEBUG -debug+ -out:"${CS_GENERATED_LIB_DIR}\\lib\\SwinGame.dll" "${CS_LIBRARY_DIR}\\*.cs" "${CS_GENERATED_CODE_DIR}\\*.cs"
else
    gmcs -t:library -r:System.Drawing.dll -define:DEBUG -debug+ -out:"${CS_GENERATED_LIB_DIR}/lib/SwinGame.dll" "${CS_LIBRARY_DIR}/*.cs" "${CS_GENERATED_CODE_DIR}/*.cs"
fi

#
# Step 8: Copy files
#
echo "  ... Copy C# templates"
DoDist "${COPY_LIST}" "${CS_DIST_DIR}" "${CS_GENERATED_LIB_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_CS_TEMPLATE_DIR}"

echo "  ... Copy VB templates"
DoDist "${VB_COPY_LIST}" "${VB_DIST_DIR}" "${CS_GENERATED_LIB_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_VB_TEMPLATE_DIR}"

echo "  Finished"
echo "--------------------------------------------------"


