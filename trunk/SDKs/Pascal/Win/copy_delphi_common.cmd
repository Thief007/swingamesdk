@echo off

if not exist .\Delphi\Resources mkdir .\Delphi\Resources

copy ..\..\Base\All\* .\Delphi\
xcopy ..\..\Base\All\Resources\* .\Delphi\Resources /e /y /q
xcopy ..\..\Base\Pascal\*.pas .\Delphi /e /y /q