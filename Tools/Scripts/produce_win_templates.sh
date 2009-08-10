#!/bin/bash

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

source "${APP_PATH}/inc/version.sh"
source "${APP_PATH}/inc/base_template_dirs.sh"

echo -n "Is ${SG_VERSION} the correct version number? [yn]: "
read answer
if [ "a${answer}a" != "aya" ] ; then
    echo "Please update version.sh and retry."
    exit 1
fi

echo -n "Clean all and recreate templates? [yn]: "
read answer

if [ "a${answer}a" == "aya" ] ; then
    ./clean_all.sh
    ./create_library.sh
    ./bundle_c_templates.sh
    ./bundle_cs_templates.sh
    #./bundle_objc_templates.sh
    ./bundle_pas_templates.sh
fi

WIN_DMG_LIST=( "C GCC,${GCC_C_DIST_DIR},Project Template")
WIN_DMG_LIST=( "${WIN_DMG_LIST[@]}" "C CodeBlocks,${CODEBLOCKS_C_DIST_DIR},")
#WIN_DMG_LIST=( "${WIN_DMG_LIST[@]}" "ObjC GCC,${GCC_OBJC_DIST_DIR},Project Template")
#WIN_DMG_LIST=( "${WIN_DMG_LIST[@]}" "Mono C#,${MONO_DIST_DIR},Project Template")
WIN_DMG_LIST=( "${WIN_DMG_LIST[@]}" "FPC,${FPC_PAS_DIST_DIR},Project Template")
#WIN_DMG_LIST=( "${WIN_DMG_LIST[@]}" "Source,${SOURCE_DIST_DIR},SwinGame")

#Simple copy list with VS templates
SMPL_COPY_LIST=( "C# Express 08,${STUDIO_EX_CS_08_TEMP_DIR},${STUDIO_EX_CS_08_DIST_DIR}" )


source ${APP_PATH}/inc/copy_without_svn.sh

ZIP_TMP_DIR="${TMP_DIR}/Zips"
rm -rf ${ZIP_TMP_DIR}

echo "  ... Creating zip files"
for arg in "${WIN_DMG_LIST[@]}"; do
    name=`echo $arg | awk -F"," '{print $1}'`
    from=`echo $arg | awk -F"," '{print $2}'`
    subdir=`echo $arg | awk -F"," '{print $3}'`
    
    ZIP_ROOT="${ZIP_TMP_DIR}/SwinGame ${SG_VERSION} ${name}"
    
    if [ "aa" == "a${subdir}a" ] ; then
        ZIP_BASE_DIR="${ZIP_ROOT}"
    else
        ZIP_BASE_DIR="${ZIP_ROOT}/${subdir}"
    fi
    
    create_from="${ZIP_TMP_DIR}/SwinGame ${SG_VERSION} ${name}"
    
    to="${DIST_DIR}/SwinGame ${SG_VERSION} ${name}.zip"
    echo "  ... ${to}"
    
    mkdir -p "${ZIP_BASE_DIR}"
    cp -r "${from}"/* "${ZIP_BASE_DIR}"
    
    cd "${ZIP_ROOT}"
    
    if [ -f "${to}" ] ; then
       rm "${to}" 
    fi
    
    zip -q -r "${to}" *
    
    rm -rf "${ZIP_BASE_DIR}"
done

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
mv "SwinGame C# Template Installer.vsi" "${DIST_DIR}"


# # Create ZIPs
# echo "  ... Creating Code blocks zip"
# cd "${C_DIST_DIR}/code blocks"
# zip -r "../SwinGame C - Code Blocks.zip" "*" > /dev/null
# 
# echo "  ... Creating GCC zip"
# cd "${GCC_C_DIST_DIR}"
# zip -r "../SwinGame C - GCC.zip" "*" > /dev/null

rm -rf ${ZIP_TMP_DIR}

echo " ... Done"

