#!/bin/sh

Usage()
{
	echo "Usage: $0 productName"
	exit 1
}

while getopts h:? o
do
	case "$o" in
	h) Usage;;
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

echo "Building $PRODUCT_NAME"

echo Compiling game

fpc -XMSDL_main -Mdelphi -FE./bin -Fu./lib/Pas -FU./bin -o"$PRODUCT_NAME" GameLauncher.pas 

echo Copying resources

cp -r Resources bin/

rm ./bin/*.o ./bin/*.ppu
