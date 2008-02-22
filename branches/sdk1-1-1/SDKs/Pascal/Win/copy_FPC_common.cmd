@echo off

if not exist .\FPC\Resources mkdir .\FPC\Resources

cp -R -u ..\..\Base\All\Resources\* .\FPC\Resources
cp -R -u ..\..\Base\Pascal\*.pas .\FPC