unit sgDriverInput;
interface

uses sgDriverInputSDL, sgTypes;


type

  IsKeyPressedProcedure = function(virtKeyCode: Longint) : Boolean;
  CheckQuitProcedure = function() : Boolean;
  ProcessEventsProcedure = procedure();
  DestroyProcedure = procedure();  
  GetKeyStateProcedure = function() : Byte;
  GetRelativeMouseStateProcedure = function(var x : LongInt; var y : LongInt): Byte;
    
  InputDriverRecord = Record
    IsKeyPressed : IsKeyPressedProcedure;
    CheckQuit : CheckQuitProcedure;
    ProcessEvents : ProcessEventsProcedure;
    Destroy : DestroyProcedure;
    GetKeyState : GetKeyStateProcedure;
    GetRelativeMouseState : GetRelativeMouseStateProcedure;
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
  
  procedure DefaultDestroyProcedure();
  begin
    LoadDefaultInputDriver();
    InputDriver.Destroy();
  end;
  

  
  function DefaultGetKeyStateProcedure() : Byte; 
  begin
    LoadDefaultInputDriver();
    result := InputDriver.GetKeyState();
  end;
  
  function DefaultGetRelativeMouseStateProcedure(var x : LongInt; var y : LongInt) : Byte;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.GetRelativeMouseState(x,y);
  end;

initialization
  begin
    InputDriver.IsKeyPressed := @DefaultIsKeyPressedProcedure;
    InputDriver.CheckQuit := @DefaultCheckQuitProcedure;
    InputDriver.ProcessEvents := @DefaultProcessEventsProcedure;
    InputDriver.Destroy := @DefaultDestroyProcedure;
    InputDriver.GetKeyState := @DefaultGetKeyStateProcedure;
    InputDriver.GetRelativeMouseState := @DefaultGetRelativeMouseStateProcedure;
  end;



end.