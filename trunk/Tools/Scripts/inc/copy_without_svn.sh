#!/bin/sh

#
# This script contains the sh script functions related to copying
# files without .svn details for SwinGame
#

# copy files from $1 to $1 without copying SVN details
copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 -type d ! -path \*.svn\* ! -path \*/. -exec mkdir "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

# copy frameworks from $1 to $1 without copying SVN details
copyFrameworksWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . ! -path \*.svn\* ! -path \*/. -mindepth 1 -type d -path \*.framework\* -exec mkdir "${TO_DIR}/{}" \;
    # Copy files
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}
