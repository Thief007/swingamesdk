@echo off

if not exist .\Delphi\resources mkdir .\Delphi\resources

cp -R -u ..\..\Base\All\resources\* .\Delphi\resources
cp -R -u ..\..\Base\Pascal\*.pas .\Delphi