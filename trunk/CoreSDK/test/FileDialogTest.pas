program FileDialogTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  Math, SysUtils,
  sgUtils,
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, sgImages, sgInput;

procedure InitInterface();
begin
  DrawGUIAsVectors(true);
end;

procedure Main();
var
  lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('File Dialog Test', 800, 600);
  
  InitInterface();
  ShowOpenDialog(fdFiles);
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    UpdateInterface();
    
    DrawFramerate(0,0);
    RefreshScreen();
    
    if KeyTyped(vk_s) then TakeScreenShot('FileDialogTest');
  until WindowCloseRequested() or DialogComplete() or DialogCancelled();
  
  WriteLn(DialogPath());
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
