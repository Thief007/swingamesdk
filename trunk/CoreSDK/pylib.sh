#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 

BaseDir="$APP_PATH"
PythonSDK="$APP_PATH/../SDKs/Python/All"
PythonSDK=`cd "$PythonSDK"; pwd`
Output="$BaseDir/Python/lib"
Libs=

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
	/usr/bin/libtool  -arch_only ${1} -dynamic -L"${Output}" -search_paths_first -multiply_defined suppress -o "$Output/libSGSDK${1}.dylib" `cat ./src/maclink${1}.res` `cat ./src/pylink.res` -current_version 1.1.1
	if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
		
	rm "${Output}"/*.o 2>> /dev/null
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

echo "__________________________________________________"
echo "Building Python version"
echo "__________________________________________________"
echo "  Running script from $BaseDir"
echo "  Saving output to $Output"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	Libs=${BaseDir}/lib/mac
    echo "  Copying Frameworks from $Libs"
fi

echo "  Copy to SDK $PythonSDK"
echo "__________________________________________________"

echo "  ... Creating py fies"
cd "$APP_PATH"
cp ./src/sgsdk.pas ./Python
cd ./Python
python sgsdk_pas_to_py.py
rm -f sgsdk.pas
cd ..

if [ ! -e $PythonSDK/sgsdk ]
then
    mkdir -p $PythonSDK/sgsdk
fi

cp ./Python/__init__.py $PythonSDK/sgsdk
cp ./Python/sgsdk.py $PythonSDK/sgsdk
cp ./Python/sgsdk_types.py $PythonSDK/sgsdk
cp ./Python/sgsdk_resources.py $PythonSDK/sgsdk

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	echo "  ... Compiling Library"

	FPC_BIN=`which ppcppc`
	doCompile "ppc"
	
	FPC_BIN=`which ppc386`
	doCompile "i386"
	
	doLipo "i386" "ppc"
else
    fpc -v0 -g -Mdelphi $EXTRA_OPTS -FE"$Output" ./src/SGSDK.pas >> out.log
    if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${BaseDir}/out.log; exit 1; fi

    rm "$Output"/*.o
    rm "$Output"/*.ppu	
fi

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
    cp $Output/*.dylib $PythonSDK/sgsdk
    pushd . > /dev/null
	cd "${Libs}"

    if [ -d "$PythonSDK/Frameworks/" ]
    then
    	rm -rf "$PythonSDK/Frameworks/"
    fi
    mkdir -p "$PythonSDK/Frameworks/"

	#
	# Copying frameworks ... files to SDK
	#
	echo "  ... Adding private Frameworks"
	find . -type d \! -path *.svn* \! -name . -exec mkdir "$PythonSDK/Frameworks/{}" \;		
	find . \! -type d \! -path *.svn* -exec cp -R {} "$PythonSDK/Frameworks/{}" \;
						
	popd > /dev/null		
	
else
    cp $Output/*.so $PythonSDK/sgsdk
fi

