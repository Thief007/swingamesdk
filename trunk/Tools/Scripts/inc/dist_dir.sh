#!/bin/sh

#
# This script contains the sh script functions related to copying
# distributions for SwinGame
#

# List the dists in the array $1
ListDists()
{
    COPY_LIST=$1
    for arg in "${COPY_LIST[@]}"; do
        name=`echo $arg | awk -F"," '{print $1}'`
        echo "    - $name"
    done
}

# Create the dist directories = $1 is array to copy
CreateDistDirs()
{
    COPY_LIST=$1
    for arg in "${COPY_LIST[@]}"; do
        to=`echo $arg | awk -F"," '{print $3}'`
        
        if [ -d "${to}" ]; then
            echo "  ... Removing old distribution directory"
            rm -rf "${to}"
        fi
        
        mkdir -p "${to}"
    done
}

# Copy the dist directories from $1 array, with dist dir in $2, common in $3, and lang common in $4 (optional)
CopyDists()
{
    COPY_LIST=$1
    SOURCE_DIST_DIR=$2
    COMMON_TEMPLATE_DIR=$3
    COMMON_LANG_TEMPLATE_DIR=$4
    
    for arg in "${COPY_LIST[@]}"; do
        name=`echo $arg | awk -F"," '{print $1}'`
        from=`echo $arg | awk -F"," '{print $2}'`
        to=`echo $arg | awk -F"," '{print $3}'`
        
        echo -n "  ... Copying to $name"
        
        copyWithoutSVN "$COMMON_TEMPLATE_DIR" "$to"
        if [ ! "a$COMMON_LANG_TEMPLATE_DIR" = "a" ]; then
            copyWithoutSVN "$COMMON_LANG_TEMPLATE_DIR" "$to"
        fi
        copyWithoutSVN "$from" "$to"
        #mark scripts executable
        find "$to" -name \*.sh -exec chmod a+x {} \; 
        
        if [ "$OS" = "$MAC" ]; then
            echo -n " with library"
            #Copy SGSDK framework
            cp -R -p -f "${SOURCE_DIST_DIR}/bin"/*.framework "${to}/lib"
            #Copy SDL frameworks
            cp -R -p -f "${SOURCE_DIST_DIR}/lib"/*.framework "${to}/lib"
        elif [ "$OS" = "$WIN" ]; then
            echo -n " with library"
            #Copy SGSDK framework
            cp -p -f "${SOURCE_DIST_DIR}/bin"/*.dll "${to}/lib"
            cp -p -f "${SOURCE_DIST_DIR}/bin"/*.a "${to}/lib"
            #Copy SDL frameworks
            cp -p -f "${SOURCE_DIST_DIR}/lib"/*.dll "${to}/lib"
            cp -p -f "${SOURCE_DIST_DIR}/lib"/*.a "${to}/lib"
        fi
        echo ""
    done
}

# Perform the process of copying all files for a distribution
# call with arguments: "${COPY_LIST}" "${LANG_DIST_DIR}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_LANG_TEMPLATE_DIR}"
DoDist()
{
    COPY_LIST=$1
    LANG_DIST_DIR=$2
    SOURCE_DIST_DIR=$3
    COMMON_TEMPLATE_DIR=$4
    COMMON_LANG_TEMPLATE_DIR=$5
    
    #Step 1: Delete old dists if they exist
    if [ -d "${LANG_DIST_DIR}" ]; then
        echo "  ... Removing old distribution directory"
        rm -rf "${LANG_DIST_DIR}"
    fi
    
    #Step 6.2: Create dists
    echo "  ... Creating distribution directories"
    CreateDistDirs "${COPY_LIST}"
    
    #Step 4: Copy files to all
    echo "  ... Copying files"
    CopyDists "${COPY_LIST}" "${SOURCE_DIST_DIR}" "${COMMON_TEMPLATE_DIR}" "${COMMON_LANG_TEMPLATE_DIR}"
}
