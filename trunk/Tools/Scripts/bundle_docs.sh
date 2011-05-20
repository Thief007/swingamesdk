#!/bin/sh

# ======================================================================
# = Bundle the Documentation files produced by create_documentation.py =
# ======================================================================

#
# Step 1: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Step 2: Detect the operating system, SwinGame version, and directories
#
source "${APP_PATH}/inc/version.sh"
source "${APP_PATH}/inc/os_check.sh"
source "${APP_PATH}/inc/base_template_dirs.sh"
source "${APP_PATH}/inc/copy_without_svn.sh"

echo "--------------------------------------------------"
echo "          Creating Source Template"
echo "                for OS: $OS"
echo "              SwinGame: ${SG_VERSION}"
echo "--------------------------------------------------"
echo " Template: ${DOC_TEMPLATE_DIR}"
echo " Generated: ${DOC_GENERATED_DIR}"
echo " Saving to: ${DOC_DIST_DIR}"
echo "--------------------------------------------------"

#
# Step 3: Run the python scripts
# 
echo " Generating Documentation..."
cd "${PYTHON_SCRIPT_DIR}"
python create_documentation.py

#
# Step 4: Prepare dist directory
#
if [ ! -d "${DOC_DIST_DIR}" ]
then
    rm -rf "${DOC_DIST_DIR}"
fi

mkdir -p "${DOC_DIST_DIR}"

#
# Step 5: Copy the files from the generated and template to dist
#
echo " Copying Generated..."
copyWithoutSVN ${DOC_GENERATED_DIR} ${DOC_DIST_DIR}
echo " Copying Template..."
copyWithoutSVN ${DOC_TEMPLATE_DIR} ${DOC_DIST_DIR}

echo "Done.."
