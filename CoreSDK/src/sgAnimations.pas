//=============================================================================
// sgAnimation.pas
//=============================================================================
//
// The Animation unit is responsible for defining and managing animation
// information for SwinGame.
//
// Change History:
//
// Version 3:
// - 2009-12-08: Andrew : Created
//=============================================================================

{$I sgTrace.inc}

unit sgAnimations;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  //----------------------------------------------------------------------------
  // Creating an AnimationTemplate
  //----------------------------------------------------------------------------
  
  /// Load animation details from a animation frames file.
  ///
  /// @lib
  /// @sn animationFramesFromFile:%s
  ///
  /// @class AnimationTemplate
  /// @constructor
  /// @csn initFromFile:%s
  function LoadAnimationTemplate(filename: String) : AnimationTemplate;
  
  /// Frees loaded animation frames data. Use this when you will no longer be 
  /// using the animation for any purpose, including within sprites.
  ///
  /// @lib
  ///
  /// @class AnimationTemplate
  /// @dispose
  procedure FreeAnimationTemplate(var framesToFree: AnimationTemplate);
  
  
//----------------------------------------------------------------------------
// AnimationTemplate mapping routines
//----------------------------------------------------------------------------
  
  /// Loads and returns a AnimationTemplate. The supplied `filename` is used to
  /// locate the AnimationTemplate to load. The supplied `name` indicates the 
  /// name to use to refer to this in SwinGame. The `AnimationTemplate` can then be
  /// retrieved by passing this `name` to the `FetchAnimationTemplate` function. 
  ///
  /// @lib
  ///
  /// @sn animationFramesNamed:%s fromFile:%s
  ///
  /// @class AnimationTemplate
  /// @constructor
  /// @csn initWithName:%s forFile:%s
  function MapAnimationTemplate(name, filename: String): AnimationTemplate;
  
  /// Determines if SwinGame has animation frames loaded for the supplied name.
  /// This checks against all loaded animation frames, those loaded without a name
  /// are assigned the filename as a default.
  ///
  /// @lib
  function HasAnimationTemplate(name: String): Boolean;
  
  /// Returns the `AnimationTemplate` that has been loaded with the specified name,
  /// see `MapAnimationTemplate`.
  ///
  /// @lib
  function FetchAnimationTemplate(name: String): AnimationTemplate;
  
  /// Releases the SwinGame resources associated with the animation frames of the
  /// specified `name`.
  ///
  /// @lib
  procedure ReleaseAnimationTemplate(name: String);
  
  /// Releases all of the bitmaps that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllAnimationTemplates();
  
  
  //----------------------------------------------------------------------------
  // Creating an Animation
  //----------------------------------------------------------------------------
  
  /// Creates an animation from a KeyFrameTemplate.
  ///
  /// @lib
  /// @sn animation:%s from:%s
  ///
  /// @class Animation
  /// @constructor
  /// @csn initAs:%s from:%s
  function CreateAnimation(identifier: String;  frames: AnimationTemplate): Animation;
  
//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils,   // libsrc
    SDL_Mixer, SDL,           // SDL
    sgShared, sgResources, sgTrace;
//=============================================================================

var
  _Animations: TStringHash;

function DoLoadAnimationTemplate(name, filename: String) : AnimationTemplate;
type RowData = record
    id,cell,dur,next: LongInt;
  end;
var
  rows: Array of RowData;
  input: Text; //the bundle file
  delim: TSysCharSet;
  line, id, data, path, temp: String;
  lineNo, maxId: Integer;
  
  function MyStrToInt(str: String) : LongInt;
  begin
    if not TryStrToInt(str, result) then
    begin
      result := 0;
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Value is not an integer : ' + str);
    end
    else if result < 0 then
    begin
      result := 0;
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Values should be positive : ' + str);
    end;
  end;
  
  procedure AddRow(myRow: RowData);
  var
    j: LongInt;
  begin
    // Check if beyond current range
    if myRow.id > maxId then
    begin
      SetLength(rows, myRow.id + 1);
      
      // Mark new rows as "empty"
      for j := maxId + 1 to High(rows) do
      begin
        rows[j].id := -1;
      end;
      
      maxId := myRow.id;
    end;
    
    if rows[myRow.id].id <> -1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A frame with id ' + IntToStr(myRow.id) + ' already exists.');
      exit;
    end
    else
    begin
      // Success add the row.
      rows[myRow.id] := myRow;
    end;
  end;
  
  procedure ProcessFrame();
  var
    myRow: RowData;
  begin
    if CountDelimiter(data, ',') <> 3 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A frame must have 4 values separated as id,cell,dur,next');
      exit;
    end;
    myRow.id := MyStrToInt(ExtractDelimited(1,data,[',']));
    myRow.cell := MyStrToInt(ExtractDelimited(2,data,[',']));
    myRow.dur := MyStrToInt(ExtractDelimited(3,data,[',']));
    myRow.next := MyStrToInt(ExtractDelimited(4,data,[',']));
    
    AddRow(myRow);
  end;
  
  procedure ProcessMultiFrame();
  begin
    WriteLn('Process mf ', data);
  end;
  
  procedure ProcessId();
  begin
    WriteLn('Process id ', data);
  end;
  
  procedure ProcessSound();
  begin
    WriteLn('Process sound ', data);
  end;
  
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimation', 'LoadAnimationTemplate');
  {$ENDIF}
  
  path := FilenameToResource(name, AnimationResource);
  lineNo := 0;
  maxId := -1;
  SetLength(rows, 0);
  result := nil;
  
  Assign(input, path);
  Reset(input);
  
  while not EOF(input) do
  begin
    lineNo := lineNo + 1;
    
    ReadLn(input, line);
    line := Trim(line);
    if Length(line) = 0 then continue;  //skip empty lines
    if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
    
    id := ExtractDelimited(1, line, [':']);
    data := ExtractDelimited(2, line, [':']);
    if Length(id) <> 1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be a single character.');
      exit;
    end;
    
    case LowerCase(id)[1] of // in all cases the data variable is read
      'f': ProcessFrame();
      'm': ProcessMultiFrame();
      'i': ProcessId();
      's': ProcessSound();
      else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be one of f,m,i or s.')
      end;
    end;
    
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgAnimation', 'LoadAnimationTemplate');
  {$ENDIF}  
end;

function LoadAnimationTemplate(filename: String) : AnimationTemplate;
begin
  result := MapAnimationTemplate(filename, filename);
end;

procedure FreeAnimationTemplate(var framesToFree: AnimationTemplate);
begin
  ReleaseAnimationTemplate(framesToFree^.name);
  framesToFree := nil;
end;

function MapAnimationTemplate(name, filename: String): AnimationTemplate;
var
  obj: tResourceContainer;
  frm: AnimationTemplate;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAudio', 'MapAnimationFrame', name + ' = ' + filename);
  {$ENDIF}
  
  if _Animations.containsKey(name) then
  begin
    RaiseException('Error loaded AnimationTemplate resource twice, ' + name);
    result := nil;
    exit;
  end;
  
  frm := DoLoadAnimationTemplate(filename, name);
  obj := tResourceContainer.Create(frm);
  
  if not _Animations.setValue(name, obj) then
  begin
    RaiseException('** Leaking: Caused by loading AnimationTemplate resource twice, ' + name);
    result := nil;
    exit;
  end;
  result := frm;
  
  {$IFDEF TRACE}
    TraceExit('sgAudio', 'MapAnimationFrame');
  {$ENDIF}
end;

function HasAnimationTemplate(name: String): Boolean;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAudio', 'HasAnimationTemplate', name);
  {$ENDIF}
  
  result := _Animations.containsKey(name);
  
  {$IFDEF TRACE}
    TraceExit('sgAudio', 'HasAnimationTemplate', BoolToStr(result, true));
  {$ENDIF}
end;

function FetchAnimationTemplate(name: String): AnimationTemplate;
var
  tmp : TObject;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAudio', 'FetchAnimationTemplate', name);
  {$ENDIF}
  
  tmp := _Animations.values[name];
  if assigned(tmp) then result := AnimationTemplate(tResourceContainer(tmp).Resource)
  else result := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgAudio', 'FetchAnimationTemplate', HexStr(result));
  {$ENDIF}
end;

procedure DoFreeAnimationTemplate(var frm: AnimationTemplate);
var
  frmPtr: TStringHash;
begin
  frmPtr := TStringHash(frm^.data);
  frmPtr.Free();
  Dispose(frm);
  frm := nil;
end;

procedure ReleaseAnimationTemplate(name: String);
var
  frm: AnimationTemplate;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAudio', 'ReleaseAnimationTemplate', 'frm = ' + name);
  {$ENDIF}
  
  frm := FetchAnimationTemplate(name);
  
  if (assigned(frm)) then
  begin
    _Animations.remove(name).Free();
    DoFreeAnimationTemplate(frm);
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgAudio', 'ReleaseAnimationTemplate');
  {$ENDIF}
end;

procedure ReleaseAllAnimationTemplates();
begin
  {$IFDEF TRACE}
    TraceEnter('sgAudio', 'ReleaseAllAnimationTemplates', '');
  {$ENDIF}
  
  ReleaseAll(_Animations, @ReleaseAnimationTemplate);
  
  {$IFDEF TRACE}
    TraceExit('sgAudio', 'ReleaseAllAnimationTemplates');
  {$ENDIF}
end;

function CreateAnimation(identifier: String;  frames: AnimationTemplate): Animation;
begin
  result := nil;
end;

//=============================================================================
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAnimation', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _Animations := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgAnimation', 'Initialise');
    {$ENDIF}
  end;

end.
