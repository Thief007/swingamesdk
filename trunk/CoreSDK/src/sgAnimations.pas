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
// - 2009-12-08: Andrew : Changed name to AnimationTemplate
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
  
  /// Releases the SwinGame resources associated with the animation template of the
  /// specified `name`.
  ///
  /// @lib
  procedure ReleaseAnimationTemplate(name: String);
  
  /// Releases all of the animation templates that have been loaded.
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
    sgShared, sgResources, sgTrace, sgAudio;
//=============================================================================

var
  _Animations: TStringHash;

function DoLoadAnimationTemplate(name, filename: String) : AnimationTemplate;
type
  RowData = record
    id,cell,dur,next: LongInt;
    snd: SoundEffect;
  end;
  IdData = record
    name: String;
    startId: LongInt;
  end;
var
  rows: Array of RowData;
  ids: Array of IdData;
  input: Text; //the bundle file
  delim: TSysCharSet;
  line, id, data, path, temp: String;
  lineNo, maxId: Integer;
  
  function MyStrToInt(str: String; allowEmpty: Boolean) : LongInt;
  begin
    if allowEmpty and (Length(str) = 0) then
    begin
      result := -1;
    end
    else if not TryStrToInt(str, result) then
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

  procedure AddID(myId: IdData);
  var
    j: LongInt;
  begin
    // Check if name is already in ids
    
    for j := 0 to High(ids) do
    begin
      if ids[j].name = myId.name then
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. The id ' + myId.name + ' already exists.');
        exit;
      end;
    end;
    
    SetLength(ids, Length(ids) + 1);
    ids[High(ids)] := myId;
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
    myRow.id := MyStrToInt(ExtractDelimited(1,data,[',']), false);
    myRow.cell := MyStrToInt(ExtractDelimited(2,data,[',']), false);
    myRow.dur := MyStrToInt(ExtractDelimited(3,data,[',']), false);
    myRow.next := MyStrToInt(ExtractDelimited(4,data,[',']), true);
    
    AddRow(myRow);
  end;
  
  procedure ProcessMultiFrame();
  var
    id_range, cell_range: Array of LongInt;
    dur, next, j: LongInt;
    myRow: RowData;
  begin
    if CountDelimiterWithRanges(data, ',') <> 3 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A multi-frame must have 4 values separated as id-range,cell-range,dur,next');
      exit;
    end;
    
    id_range := ProcessRange(ExtractDelimitedWithRanges(1, data));
    cell_range := ProcessRange(ExtractDelimitedWithRanges(2, data));
    
    if Length(id_range) <> Length(cell_range) then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. The range of cells and ids is not the same length.');
      exit;
    end;
    
    dur := MyStrToInt(ExtractDelimited(3,data,[',']), false);
    next := MyStrToInt(ExtractDelimited(4,data,[',']), true);
    
    for j := Low(id_range) to High(id_range) do
    begin
      myRow.id := id_range[j];
      myRow.cell := cell_range[j];
      myRow.dur := dur;
      if j <> High(id_range) then
        myRow.next := id_range[j + 1]
      else
        myRow.next := next;
      AddRow(myRow);
    end;
  end;
  
  procedure ProcessId();
  var
    myIdData: IdData;
  begin
    if CountDelimiterWithRanges(data, ',') <> 1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A id must have 2 values separated as name,start-id');
      exit;
    end;
    
    myIdData.name := ExtractDelimited(1,data,[',']);
    myIdData.startId := MyStrToInt(ExtractDelimited(2,data,[',']), false);
    
    AddID(myIdData);
  end;
  
  procedure ProcessSound();
  var
    id: LongInt;
    sndId: String;
  begin
    if CountDelimiter(data, ',') <> 1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A sound must have two parts id,sndfile.');
      exit;
    end;

    id := MyStrToInt(ExtractDelimited(1,data,[',']), true);
    
    sndId := ExtractDelimited(2,data,[',']);
    
    if not HasSoundEffect(sndId) then
    begin
      if MapSoundEffect(sndId, sndId) = nil then
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Cannot find sound file ' + sndId);
        exit;
      end;
    end;
    rows[id].snd := FetchSoundEffect(sndId);
  end;
  
  procedure ProcessLine();
  begin
    // Split line into id and data
    id := ExtractDelimited(1, line, [':']);
    data := ExtractDelimited(2, line, [':']);
    
    // Verify that id is a single char
    if Length(id) <> 1 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be a single character.');
      exit;
    end;
    
    // Process based on id
    case LowerCase(id)[1] of // in all cases the data variable is read
      'f': ProcessFrame();
      'm': ProcessMultiFrame();
      'i': ProcessId();
      's': ProcessSound();
      else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be one of f,m,i or s.');
        exit;
      end;
    end;
  end;
  
  procedure BuildFrameLists();
  var
    frames: Array of AnimationFrame;
    hash: TStringHash;
    j, nextIdx: LongInt;
  begin
    SetLength(frames, Length(rows));
    
    for j := 0 to High(frames) do
    begin
      // Allocte space for frames
      New(frames[j]);
    end;
    
    // Link the frames together
    for j := 0 to High(frames) do
    begin
      frames[j]^.cellIndex  := rows[j].cell;
      frames[j]^.sound      := rows[j].snd;
      frames[j]^.duration   := rows[j].dur;
      
      // Get the next id and then 
      nextIdx := rows[j].next;
      
      if nextIdx = -1 then
      begin
        //The end of a list of frames = no next
        frames[j]^.next := nil;
      end
      else if (nextIdx < 0) or (nextIdx > High(frames)) then
      begin
        FreeAnimationTemplate(result);
        RaiseException('Error in animation ' + filename + '. Error with id: ' + IntToStr(j) + '. Next is outside of available frames.');
        exit;
      end
      else
        frames[j]^.next := frames[nextIdx];
    end;
    
    hash := TStringHash.Create(False, Length(ids));
    
    for j := 0 to High(ids) do
    begin
      hash.setValue(ids[j].name, TIntegerContainer.Create(ids[j].startId));
    end;
    
    //We have the data ready, now lets create the linked lists...
    New(result);
    
    result^.name      := name;      // name taken from parameter of DoLoadAnimationTemplate
    result^.filename  := filename;  // filename also taken from parameter
    result^.data      := hash;      // Keep the hash of the names
    result^.frames    := frames;    // The frames of this animation.
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
  
  try
    while not EOF(input) do
    begin
      lineNo := lineNo + 1;
    
      ReadLn(input, line);
      line := Trim(line);
      if Length(line) = 0 then continue;  //skip empty lines
      if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
    
      ProcessLine();
    end;
  
    BuildFrameLists();
  finally
    Close(input);
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
  if Assigned(framesToFree) then
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
    TraceExit('sgAudio', 'MapAnimationFrame', HexStr(result));
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
  i: LongInt;
begin
  //WriteLn(0);
  frmPtr := TStringHash(frm^.data);
  //WriteLn(1);
  frmPtr.DeleteAll();
  frmPtr.Free();
  
  //WriteLn(2);
  for i := 0 to High(frm^.frames) do
  begin
    //WriteLn(3,' ', i);
    Dispose(frm^.frames[i]);
    frm^.frames[i] := nil;
  end;
  //WriteLn(4);
  Dispose(frm);
  //WriteLn(5);
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
