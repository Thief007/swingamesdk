@echo off

if not exist .\FPC\Resources mkdir .\Delphi\Resources

cp -R -u ..\..\Base\All\Resources\* .\Delphi\Resources