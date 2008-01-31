@echo off

if not exist .\Delphi\Resources mkdir .\Delphi\Resources

cp -R -u ..\..\Base\All\Resources\* .\Delphi\Resources
cp -R -u ..\..\Base\Pascal\*.pas .\Delphi