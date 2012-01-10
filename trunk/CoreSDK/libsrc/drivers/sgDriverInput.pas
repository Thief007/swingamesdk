unit sgDriverInput;
interface

uses sgDriverInputSDL, sgTypes;


type

  IsKeyPressedProcedure = function(virtKeyCode: Longint) : Boolean;
  CheckQuitProcedure = function() : Boolean;
  ProcessEventsProcedure = procedure();
  DrawCollectedTextProcedure = procedure(dest: Bitmap);
  ShiftDownProcedure = function(keycode : LongInt) : Boolean;
  FreeOldSurfaceProcedure = procedure();
  RenderTextSurfaceProcedure = procedure(text : String);
  DestroyProcedure = procedure();  
  StartReadingTextProcedure = procedure(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle; var tempString : String);
    
    
  InputDriverRecord = Record
    IsKeyPressed : IsKeyPressedProcedure;
    CheckQuit : CheckQuitProcedure;
    ProcessEvents : ProcessEventsProcedure;
    DrawCollectedText : DrawCollectedTextProcedure;
    ShiftDown : ShiftDownProcedure;
    FreeOldSurface : FreeOldSurfaceProcedure;
    RenderTextSurface : RenderTextSurfaceProcedure;
    StartReadingText : StartReadingTextProcedure;
    Destroy : DestroyProcedure;
  end;

var
  InputDriver : InputDriverRecord;

implementation
  procedure LoadDefaultInputDriver; 
  begin
    LoadSDLInputDriver();
  end;

  function DefaultIsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.IsKeyPressed(virtKeyCode);
  end;
  
  function DefaultCheckQuitProcedure() : Boolean;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.CheckQuit();
  end;
  
  procedure DefaultProcessEventsProcedure();
  begin
    LoadDefaultInputDriver();
    InputDriver.ProcessEvents();
  end;
  
  procedure DefaultDrawCollectedTextProcedure(dest : Bitmap);
  begin
    LoadDefaultInputDriver();
    InputDriver.DrawCollectedText(dest);
  end;
  
  function DefaultShiftDownProcedure(keycode : LongInt) : Boolean;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.ShiftDown(keyCode);
  end;
  
  procedure DefaultFreeOldSurfaceProcedure();
  begin
    LoadDefaultInputDriver();
    InputDriver.FreeOldSurface();
  end;
  
  procedure DefaultRenderTextSurfaceProcedure(text : String);
  begin
    LoadDefaultInputDriver();
    InputDriver.RenderTextSurface(text);
  end;

  
  procedure DefaultDestroyProcedure();
  begin
    LoadDefaultInputDriver();
    InputDriver.Destroy();
  end;
  
  procedure DefaultStartReadingTextProcedure(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle; var tempString : String);
  begin
    LoadDefaultInputDriver();
    InputDriver.StartReadingText(textColor,maxLength,theFont,area,tempString);
  end;

initialization
  begin
    InputDriver.IsKeyPressed := @DefaultIsKeyPressedProcedure;
    InputDriver.CheckQuit := @DefaultCheckQuitProcedure;
    InputDriver.ProcessEvents := @DefaultProcessEventsProcedure;
    InputDriver.DrawCollectedText := @DefaultDrawCollectedTextProcedure;
    InputDriver.ShiftDown := @DefaultShiftDownProcedure;
    InputDriver.FreeOldSurface := @DefaultFreeOldSurfaceProcedure;
    InputDriver.RenderTextSurface := @DefaultRenderTextSurfaceProcedure;
    InputDriver.StartReadingText := @DefaultStartReadingTextProcedure;
    InputDriver.Destroy := @DefaultDestroyProcedure;
  end;



end.