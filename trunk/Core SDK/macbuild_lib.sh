#!/bin/sh

Output=`pwd`/bin/mac/

cd lib/Pas/

if [ #0 == "-clean" ]
then
	
fi

mkdir -p "$Output"

fpc -Mdelphi -FE"$Output" SGSDK_Core.pas
fpc -Mdelphi -FE"$Output" SGSDK_Graphics.pas
fpc -Mdelphi -FE"$Output" SGSDK_Font.pas
fpc -Mdelphi -FE"$Output" SGSDK_Input.pas
fpc -Mdelphi -FE"$Output" SGSDK_Physics.pas
fpc -Mdelphi -FE"$Output" SGSDK_Audio.pas