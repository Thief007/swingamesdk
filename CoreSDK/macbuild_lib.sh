#!/bin/sh

Output=`pwd`/bin/mac/
MacFPCDir=`pwd`/../SDKs/Pascal/Mac/FPC/lib/
LibDir=`pwd`/lib/mac
CLEAN="N"
SHARED_LIB="N"
EXTRA_OPTS="-O3"

while getopts hdcs o
do
	case "$o" in
	d)  EXTRA_OPTS="-gw3 -gl -gp -godwarfsets -Ci -Co -Ct";;
	h)  Usage;;
	c)  CLEAN="Y";;
	s)  SHARED_LIB="Y";;
	esac
done

cd src/

if [ $CLEAN = "Y" ]
then
	cd "$Output"
	rm -f SGSDK_Core.*
	rm -f SGSDK_Graphics.*
	rm -f SGSDK_Font.*
	rm -f SGSDK_Input.*
	rm -f SGSDK_Physics.*
	rm -f SGSDK_Audio.*
	rm -f sdl_image.*
	rm -f sdl_mixer.*
	rm -f sdl_ttf.*
	rm -f sdl.*
	rm -f SDLEventProcessing.*
	rm -f SGSDK_KeyCodes.*
	rm -f SGSDK_MappyLoader.*
	rm -f SGSDK_Camera.*
	echo Cleaned
else
	echo "Compiling with " $EXTRA_OPTS

	mkdir -p "$Output"
	
	if [ $SHARED_LIB = "N"]
	then
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Core.pas
		if [ $? != 0 ]; then echo "Error compiling Core"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Camera.pas
		if [ $? != 0 ]; then echo "Error compiling Camera"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Graphics.pas
		if [ $? != 0 ]; then echo "Error compiling Graphics"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Font.pas
		if [ $? != 0 ]; then echo "Error compiling Font"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Input.pas
		if [ $? != 0 ]; then echo "Error compiling Input"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Physics.pas
		if [ $? != 0 ]; then echo "Error compiling Physics"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Audio.pas
		if [ $? != 0 ]; then echo "Error compiling Audio"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_KeyCodes.pas
		if [ $? != 0 ]; then echo "Error compiling KeyCodes"; exit 1; fi
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_MappyLoader.pas
		if [ $? != 0 ]; then echo "Error compiling MappyLoader"; exit 1; fi
	else
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fl./lib/mac -s SGSDK.pas
		if [ $? != 0 ]; then echo "Error compiling SGSDK"; exit 1; fi
		
		echo Assembling SGSDK
		/usr/bin/as -o "$Output"/SGSDK.o "$Output"/SGSDK.s -arch i386
		rm "$Output"/SGSDK.s
		if [ $? != 0 ]; then echo "Error assembling SGSDK"; exit 1; fi
		
		echo Linking libSGSDK.dylib
		/usr/bin/ld  -dylib -L"$LibDir" -L/usr/X11R6/lib -L/usr/lib -search_paths_first -multiply_defined suppress -L. -o "$Output"/libSGSDK.dylib `cat "$Output"/link.res` -lSDL -lSDL_mixer -lsmpeg -lSDL_image -lSDL_TTF -lSDL_gfx -lz -lstdc++ -ltiff -lpng12 -lSDLmain -logg -lfreetype -ljpeg -lvorbis -lvorbisenc -lvorbisfile -framework Cocoa -framework QuickTime -framework CoreFoundation -framework IOKit -framework AudioUnit -framework Carbon -framework OpenGL -lgcc -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib		
		rm "$Output"/link.res "$Output"/ppas.sh
		if [ $? != 0 ]; then echo "Error linking libSGSDK.dylib"; exit 1; fi
			
		/usr/bin/strip -x "$Output"/libSGSDK.dylib
		if [ $? != 0 ]; then echo "Error stripping libSGSDK.dylib"; exit 1; fi
	fi
	
	echo "Copying to FPC"
	
	mkdir -p "$MacFPCDir"
	
	cp "$Output"/*.ppu "$MacFPCDir"
	cp "$Output"/*.o "$MacFPCDir"
	cp "$LibDir"/*.a "$MacFPCDir"
	
	echo "Finished"
fi
#open "$Output"