#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

DoExitAsm ()
{ 
	echo "An error occurred while assembling $1" 
	exit 1 
}

Usage()
{
	echo "Usage: [-c] [-h]"
	echo 
	echo "Compiles the SGSDK library and the SGSDK.NET library."
	echo "Libraries are copied to the Mono SDK folder."
	echo
	echo "Options:"
	echo " -c   Perform a clean rather than a build"
	echo " -h   Show this help message "
	exit 0
}

BaseDir=`pwd`
Output="$BaseDir"/SGSDK.NET/lib
Libs=

FPC_BIN=`which fpc`

DOTNETlocn="$BaseDir"/SGSDK.NET/src
DOTNETproj=SGSDK.NET.csproj
DOTNETbin="$BaseDir"/SGSDK.NET/src/bin/Debug

SDKBase="${BaseDir}/../SDKs/DOTNet/Mono"

EXTRA_OPTS="-O3 -Sewn -vwn"

CLEAN="N"

cpToSDK()
{
	echo "  ... Copying to $1 SDK"

	if [ -d "$SDKBase/${1}/lib/" ]
	then
		rm -rf "$SDKBase/${1}/lib/"
	fi
	mkdir -p "$SDKBase/${1}/lib/"
	
	cp "$Output"/*.dll "$SDKBase/$1/lib/"
	if [ $? != 0 ]; then echo "Error copying DLL"; exit 1; fi
	
	cp "$Output"/*.XML "$SDKBase/$1/lib/"

	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		cp "$Output"/*.dylib  "$SDKBase/$1/lib"
		if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
		
		pushd . > /dev/null
		cd "${Libs}"
		
		#
		# Copying frameworks ... files to SDK
		#
		echo "  ... Adding private Frameworks"
		find . -type d \! -path *.svn* \! -name . -exec mkdir "$SDKBase/${1}/lib/{}" \;		
		find . \! -type d \! -path *.svn* -exec cp -R {} "$SDKBase/${1}/lib/{}" \;
							
		popd > /dev/null		
	else
		cp "$Output"/*.so  "$SDKBase/$1/lib"
		if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
	fi
}

#
# Compile for Mac - manually assembles and links files
# 1 is arch
doCompile()
{
	if [ -d "${Output}" ]
	then
		rm "${Output}"/*.o 2>> /dev/null
		rm "${Output}"/*.s 2>> /dev/null
		rm "${Output}"/*.ppu 2>> /dev/null
		rm "${Output}"/link.res 2>> /dev/null
		rm "${Output}"/ppas.sh 2>> /dev/null
	else
		mkdir "${Output}" 
	fi

	$FPC_BIN -Mdelphi $EXTRA_OPTS -FE"${Output}" -FU"${Output}" -s ./src/SGSDK.pas >> ${BaseDir}/out.log
	if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${BaseDir}/out.log; exit 1; fi

	#Assemble all of the .s files
	echo "  ... Assembling library for $1"
	
	for file in `find ${Output} | grep [.]s$` #`ls *.s`
	do
		/usr/bin/as -o ${file%.s}.o $file -arch $1
		if [ $? != 0 ]; then DoExitAsm $file; fi
		rm $file
	done

	echo "  ... Linking Library"
	/usr/bin/libtool  -arch_only ${1} -dynamic -L"${Output}" -search_paths_first -multiply_defined suppress -o "$Output/libSGSDK${1}.dylib" `cat ./src/maclink${1}.res` `cat ./src/dotnetlink.res` -current_version 2.0
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