#!/bin/sh

Usage()
{
	echo "Usage: $0 productName"
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

echo "Building $PRODUCT_NAME"

echo Compiling game

fpc -XMSDL_main -Mdelphi -FE./bin -Fu./lib -FU./bin -o"$PRODUCT_NAME" $EXTRA_OPTS GameLauncher.pas 

echo Copying resources

cp -r Resources bin/

rm ./bin/*.o ./bin/*.ppu
