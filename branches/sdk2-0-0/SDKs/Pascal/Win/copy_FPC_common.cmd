@echo off

if not exist .\FPC\Resources mkdir .\FPC\Resources


copy ..\..\Base\All\* .\FPC\
xcopy ..\..\Base\All\Resources\* .\FPC\Resources /e /y /q
xcopy ..\..\Base\Pascal\*.pas .\FPC /e /y /q