#!/bin/bash

#
# Declares the base template and dist directories for Swingame
# distributions.
#

SWINGAME_DIR="${APP_PATH}/../../"
SWINGAME_DIR=`cd "$SWINGAME_DIR"; pwd`

PYTHON_SCRIPT_DIR=${SWINGAME_DIR}/Tools/SGWrapperGen

# All files generated by scripts
GENERATED_DIR="${SWINGAME_DIR}/Generated"
# All files manually created to go with generated files
TEMPLATE_DIR="${SWINGAME_DIR}/Templates"
# All common files (images, etc.)
COMMON_TEMPLATE_DIR="${TEMPLATE_DIR}/Common"

# The destination for the combination of the above files
DIST_DIR="${SWINGAME_DIR}/Dist"

# A temporary working location
TMP_DIR=${SWINGAME_DIR}/tmp

# 
# Directories related to C
# 
C_GENERATED_DIR="${GENERATED_DIR}/C"
C_TEMPLATE_DIR="${TEMPLATE_DIR}/C"
C_DIST_DIR="${DIST_DIR}/C"

COMMON_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/common"

GCC_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/gcc"
GCC_C_DIST_DIR="${C_DIST_DIR}/gcc"

XCODE_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/xcode 3"
XCODE_C_DIST_DIR="${C_DIST_DIR}/xcode 3"

CODEBLOCKS_C_TEMPLATE_DIR="${C_TEMPLATE_DIR}/code blocks"
CODEBLOCKS_C_DIST_DIR="${C_DIST_DIR}/code blocks/SwinGame"

# 
# Directories related to Objective C
# 
OBJC_GENERATED_DIR="${GENERATED_DIR}/ObjC"
OBJC_TEMPLATE_DIR="${TEMPLATE_DIR}/ObjC"
OBJC_DIST_DIR="${DIST_DIR}/ObjC"

COMMON_OBJC_TEMPLATE_DIR="${OBJC_TEMPLATE_DIR}/common"

XCODE_OBJC_TEMPLATE_DIR="${OBJC_TEMPLATE_DIR}/xcode 3"
XCODE_OBJC_DIST_DIR="${OBJC_DIST_DIR}/xcode 3"

GCC_OBJC_TEMPLATE_DIR="${OBJC_TEMPLATE_DIR}/gcc"
GCC_OBJC_DIST_DIR="${OBJC_DIST_DIR}/gcc"

#
# Directories related to C# & VB
#
CS_GENERATED_DIR="${GENERATED_DIR}/CSharp"
CS_TEMPLATE_DIR="${TEMPLATE_DIR}/CSharp"
CS_DIST_DIR="${DIST_DIR}/CSharp"

CS_GENERATED_CODE_DIR="${CS_GENERATED_DIR}/Code"
CS_GENERATED_LIB_DIR="${CS_GENERATED_DIR}/Library"

VB_TEMPLATE_DIR="${TEMPLATE_DIR}/VB"
VB_DIST_DIR="${DIST_DIR}/VB"

COMMON_CS_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/Common"
COMMON_VB_TEMPLATE_DIR="${VB_TEMPLATE_DIR}/Common"

# The directory of the C# library code
CS_LIBRARY_DIR="${CS_TEMPLATE_DIR}/Library"

#Mono directories
MONO_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/Mono"
MONO_DIST_DIR="${CS_DIST_DIR}/Mono"

VB_MONO_TEMPLATE_DIR="${VB_TEMPLATE_DIR}/Mono"
VB_MONO_DIST_DIR="${VB_DIST_DIR}/Mono"

#VS05 directories
VS05_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/VS05"
VS05_DIST_DIR="${CS_DIST_DIR}/VS05"

#VS08 directories
VB_VS08_TEMPLATE_DIR="${VB_TEMPLATE_DIR}/VS08"
VB_VS08_DIST_DIR="${VB_DIST_DIR}/VS08"

VS08_TEMPLATE_DIR="${CS_TEMPLATE_DIR}/VS08"
VS08_DIST_DIR="${CS_DIST_DIR}/VS08"

#VS directories
STUDIO_TEMPLATE_DIR="${TEMPLATE_DIR}/Visual Studio"
STUDIO_DIST_DIR="${DIST_DIR}/Visual Studio"

STUDIO_EX_CS_08_TEMP_DIR="${STUDIO_TEMPLATE_DIR}/Express C# 08"
STUDIO_EX_CS_08_DIST_DIR="${STUDIO_DIST_DIR}/Express C# 08"

STUDIO_EX_VB_08_TEMP_DIR="${STUDIO_TEMPLATE_DIR}/Express VB 08"
STUDIO_EX_VB_08_DIST_DIR="${STUDIO_DIST_DIR}/Express VB 08"

#
# Directories related to Pascal
#
PAS_TEMPLATE_DIR="${TEMPLATE_DIR}/Pascal"
PAS_DIST_DIR="${DIST_DIR}/Pascal"
PAS_GENERATED_DIR="${GENERATED_DIR}/Pascal"

COMMON_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/Common"

FPC_PAS_TEMPLATE_DIR="${PAS_TEMPLATE_DIR}/FPC"
FPC_PAS_DIST_DIR="${PAS_DIST_DIR}/FPC"

# 
# Directories related to Source distribution
# 
SOURCE_DIST_DIR="${DIST_DIR}/Source"

