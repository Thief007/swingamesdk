#!/bin/sh

Usage()
{
	echo Usage: $0 [-d] [-h] productName
	echo
	echo Compile the Pascal SwinGame with name "productName"
	echo
	echo Options:
	echo   -d = Debug
	echo   -h = Show usage
	exit 1
}

while getopts hd? o
do
	case "$o" in
	h) Usage;;
	d) EXTRA_OPTS="-g -gl -gc";;
	[?]) Usage;;
	esac
done

shift $((${OPTIND}-1))

if [ -n "$1" ] 
then
	PRODUCT_NAME="$1"
else
	Usage
fi

if [ ! -d ./bin ]
then
	echo "Creating bin folder"
	mkdir ./bin
fi

echo __________________________________________________
echo Building Linux version
echo __________________________________________________	
echo   Product Name $PRODUCT_NAME
echo   Executable name $EXECUTABLE_NAME
echo   Compiling with $EXTRA_OPTS

echo   ... Compiling game

fpc -XMSDL_main -Mdelphi -FE./bin -Fu./lib -FU./bin -o"$PRODUCT_NAME" $EXTRA_OPTS GameLauncher.pas 

echo   ... Copying resources

cp -r Resources bin/

rm ./bin/*.o ./bin/*.ppu
echo    Finished