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
// - 2009-12-15: Andrew : Updated animation handling to use new NamedIndexCollection.
//                      : Fixed loading of animations with ranges like [,]
//                      : Added code to query animations, and more create code.
// - 2009-12-10: Andrew : Got basic Animation features working
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
  
  /// Creates an animation from a AnimationTemplate. This may play a sound effect
  /// if the animation is set to play a sound effect on its first frame.
  ///
  /// @lib
  /// @sn animationNamed:%s from:%s
  ///
  /// @class Animation
  /// @constructor
  /// @csn initAsName:%s from:%s
  function CreateAnimation(identifier: String;  frames: AnimationTemplate): Animation; overload;
  
  /// Creates an animation from a AnimationTemplate. If withSound is true, this may
  /// play a sound effect if the animation is set to play a sound effect on its first frame.
  ///
  /// @lib
  /// @sn animationNamed:%s from:%s withSound:%s
  ///
  /// @class Animation
  /// @constructor
  /// @csn initAsName:%s from:%s withSound:%s
  function CreateAnimation(identifier: String;  frames: AnimationTemplate; withSound: Boolean): Animation;
  
  /// Creates an animation from a AnimationTemplate. If withSound is true, this may
  /// play a sound effect if the animation is set to play a sound effect on its first frame.
  ///
  /// @lib
  /// @sn animationAtIndex:%s from:%s withSound:%s
  ///
  /// @class Animation
  /// @constructor
  /// @csn initAtIndex:%s from:%s withSound:%s
  function CreateAnimation(identifier: LongInt;  frames: AnimationTemplate; withSound: Boolean): Animation; overload;
  
  /// Creates an animation from a AnimationTemplate. This may play a sound effect
  /// if the animation is set to play a sound effect on its first frame.
  ///
  /// @lib
  /// @sn animationAtIndex:%s from:%s
  ///
  /// @class Animation
  /// @constructor
  /// @csn initAtIndex:%s from:%s
  function CreateAnimation(identifier: LongInt;  frames: AnimationTemplate): Animation; overload;
  
  
  //----------------------------------------------------------------------------
  // Drawing Animations
  //----------------------------------------------------------------------------
  
  /// Uses the animation information to draw a bitmap at the specified
  /// x,y location.
  ///
  /// @lib
  /// @sn drawAnimation:%s bitmap:%s x:%s y:%s
  ///
  /// @class Animation
  /// @method DrawBitmap
  /// @csn drawBitmap:%s x:%s y:%s
  procedure DrawAnimation(ani: Animation; bmp: Bitmap; x,y: LongInt); overload;
  
  /// Uses the animation information to draw a bitmap at the specified
  /// point.
  ///
  /// @lib
  /// @sn drawAnimation:%s bitmap:%s pt:%s
  ///
  /// @class Animation
  /// @overload DrawBitmap DrawBitmapAtPt
  /// @csn drawBitmap:%s pt:%s
  procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  
  /// Uses the animation information to draw a bitmap at the specified
  /// x,y location on a destination bitmap.
  ///
  /// @lib
  /// @sn drawOnto:%s animation:%s bitmap:%s x:%s y:%s
  ///
  /// @class Animation
  /// @overload DrawBitmap DrawBitmapOnto
  /// @self 2
  /// @csn drawOnto:%s bitmap:%s x:%s y:%s
  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x,y: LongInt); overload;

  /// Uses the animation information to draw a bitmap at the specified
  /// point on a destination bitmap.
  ///
  /// @lib
  /// @sn drawOnto:%s animation:%s bitmap:%s pt:%s
  ///
  /// @class Animation
  /// @overload DrawBitmap DrawBitmapAtPtOnto
  /// @csn drawOnto:%s using:%s pt:%s
  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  
  /// Uses the animation information to draw a bitmap to the screen at the specified
  /// x,y location.
  ///
  /// @lib
  /// @sn drawAnimation:%s bitmap:%s onScreenAtX:%s y:%s
  ///
  /// @class Animation
  /// @method DrawBitmapOnScreen
  /// @csn drawBitmap:%s onScreenAtX:%s y:%s
  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x,y: LongInt); overload;
  
  /// Uses the animation information to draw a bitmap to the screen at the specified
  /// point.
  ///
  /// @lib
  /// @sn drawAnimation:%s bitmap:%s onScreenAtPt:%s
  ///
  /// @class Animation
  /// @overload DrawBitmapOnScreen DrawBitmapAtPtOnScreen
  /// @csn drawBitmap:%s onScreenAtPt:%s
  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  
  
  //----------------------------------------------------------------------------
  // Updating Animations
  //----------------------------------------------------------------------------
  
  /// Updates the animation, updating the time spent and possibly moving to a new
  /// frame in the animation. This may play a sound effect if the new frame
  /// triggers a sound.
  /// 
  /// @lib
  /// @sn updateAnimation:%s
  ///
  /// @class Animation
  /// @method Update
  /// @csn update
  procedure UpdateAnimation(anim: Animation); overload;
  
  /// Updates the animation a certain percentage and possibly moving to a new
  /// frame in the animation. This may play a sound effect if the new frame
  /// triggers a sound.
  /// 
  /// @lib UpdatePct
  /// @sn updateAnimation:%s pct:%s
  ///
  /// @class Animation
  /// @overload Update UpdatePct
  /// @csn updatePct:%s
  procedure UpdateAnimation(anim: Animation; pct: Single); overload;
  
  /// Updates the animation a certain percentage and possibly moving to a new
  /// frame in the animation. This may play a sound effect if the new frame
  /// triggers a sound and withSound is true.
  /// 
  /// @lib UpdatePctAndSound
  /// @sn updateAnimation:%s pct:%s withSound:%s
  /// 
  /// @class Animation
  /// @overload Update UpdatePctAndSound
  /// @csn updatePct:%s withSound:%s
  procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
  
  
  //----------------------------------------------------------------------------
  // Query Animation
  //----------------------------------------------------------------------------
  
  /// Indicates if an animation has ended. Animations with loops will never end.
  /// 
  /// @lib
  ///
  /// @class Animation
  /// @getter Ended
  function AnimationEnded(anim: Animation): Boolean;
  
  /// Returns the current cell (the part of the image or sprite) of this animation.
  /// This can be used to animate an image or sprite.
  ///
  /// @lib
  ///
  /// @class Animation
  /// @getter CurrentCell
  function AnimationCurrentCell(anim: Animation): LongInt;
  
  /// Returns true if the animation entered a new frame on its last update.
  /// This can be used to trigger actions on frames within an animation.
  ///
  /// @lib
  /// 
  /// @class Animation
  /// @getter EnteredFrame
  function AnimationEnteredFrame(anim: Animation): Boolean;
  
  /// Returns the amount of time spent in the current frame. When this exceeds
  /// the frame's duration the animation moves to the next frame.
  /// 
  /// @lib
  /// 
  /// @class Animation
  /// @getter FrameTime
  function AnimationFrameTime(anim: Animation): Single;
  
  
//=============================================================================
implementation
  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection,   // libsrc
    SDL_Mixer, SDL,           // SDL
    sgShared, sgResources, sgTrace, sgAudio, sgImages;
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
  line, id, data, path: String;
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
        rows[j].snd := nil;
        rows[j].cell := -1;
        rows[j].next := -1;
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
    myRow.id    := MyStrToInt(ExtractDelimited(1,data,[',']), false);
    myRow.cell  := MyStrToInt(ExtractDelimited(2,data,[',']), false);
    myRow.dur   := MyStrToInt(ExtractDelimited(3,data,[',']), false);
    myRow.next  := MyStrToInt(ExtractDelimited(4,data,[',']), true);
    myRow.snd   := nil;
    
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
    
    dur := MyStrToInt(ExtractDelimitedWithRanges(3,data), false);
    next := MyStrToInt(ExtractDelimitedWithRanges(4,data), true);
    
    for j := Low(id_range) to High(id_range) do
    begin
      myRow.id    := id_range[j];
      myRow.cell  := cell_range[j];
      myRow.dur   := dur;
      myRow.snd   := nil;
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
    j, nextIdx, addedIdx: LongInt;
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
    
    //We have the data ready, now lets create the linked lists...
    New(result);
    
    result^.name      := name;      // name taken from parameter of DoLoadAnimationTemplate
    result^.filename  := filename;  // filename also taken from parameter
    result^.frames    := frames;    // The frames of this animation.
    
    SetLength(result^.animations, Length(ids));
    InitNamedIndexCollection(result^.animationIds);   //Setup the name <-> id mappings
    
    for j := 0 to High(ids) do                        //Add in the animation starting indexes
    begin
      addedIdx := AddName(result^.animationIds, ids[j].name);   //Allocate the index
      result^.animations[addedIdx] := ids[j].startId;           //Store the linked index
    end;
  end;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'LoadAnimationTemplate');
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
    TraceExit('sgAnimations', 'LoadAnimationTemplate');
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
    TraceEnter('sgAnimations', 'MapAnimationFrame', name + ' = ' + filename);
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
    TraceExit('sgAnimations', 'MapAnimationFrame', HexStr(result));
  {$ENDIF}
end;

function HasAnimationTemplate(name: String): Boolean;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'HasAnimationTemplate', name);
  {$ENDIF}
  
  result := _Animations.containsKey(name);
  
  {$IFDEF TRACE}
    TraceExit('sgAnimations', 'HasAnimationTemplate', BoolToStr(result, true));
  {$ENDIF}
end;

function FetchAnimationTemplate(name: String): AnimationTemplate;
var
  tmp : TObject;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'FetchAnimationTemplate', name);
  {$ENDIF}
  
  tmp := _Animations.values[name];
  if assigned(tmp) then result := AnimationTemplate(tResourceContainer(tmp).Resource)
  else result := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgAnimations', 'FetchAnimationTemplate', HexStr(result));
  {$ENDIF}
end;

procedure DoFreeAnimationTemplate(var frm: AnimationTemplate);
var
  i: LongInt;
begin
  FreeNamedIndexCollection(frm^.animationIds);
  
  for i := 0 to High(frm^.frames) do
  begin
    Dispose(frm^.frames[i]);
    frm^.frames[i] := nil;
  end;
  Dispose(frm);
  frm := nil;
end;

procedure ReleaseAnimationTemplate(name: String);
var
  frm: AnimationTemplate;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'ReleaseAnimationTemplate', 'frm = ' + name);
  {$ENDIF}
  
  frm := FetchAnimationTemplate(name);
  
  if (assigned(frm)) then
  begin
    _Animations.remove(name).Free();
    DoFreeAnimationTemplate(frm);
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgAnimations', 'ReleaseAnimationTemplate');
  {$ENDIF}
end;

procedure ReleaseAllAnimationTemplates();
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'ReleaseAllAnimationTemplates', '');
  {$ENDIF}
  
  ReleaseAll(_Animations, @ReleaseAnimationTemplate);
  
  {$IFDEF TRACE}
    TraceExit('sgAnimations', 'ReleaseAllAnimationTemplates');
  {$ENDIF}
end;

function StartFrame(id: LongInt; temp: AnimationTemplate) : AnimationFrame;
begin
  result := nil;
  if temp = nil then exit;
  if (id < 0) or (id > High(temp^.animations)) then exit;
  
  result := temp^.frames[id];
  
end;

//----------------------------------------------------------------------------

function StartFrame(name: String; temp: AnimationTemplate) : AnimationFrame;
begin
  if assigned(temp) then
    result := StartFrame(IndexOf(temp^.animationIds, name), temp)
  else
    result := nil;
end;

function CreateAnimation(identifier: LongInt;  frames: AnimationTemplate; withSound: Boolean): Animation; overload;
begin
  result := nil;
  if frames = nil then exit;
  
  new(result);
  result^.firstFrame    := frames^.frames[identifier];
  result^.currentFrame  := result^.firstFrame;
  result^.lastFrame     := result^.firstFrame;
  result^.frameTime     := 0;
  result^.enteredFrame  := true;
  // result^.hasEnded      := false;
  
  if assigned(result^.currentFrame) and assigned(result^.currentFrame^.sound) and withSound then
    PlaySoundEffect(result^.currentFrame^.sound);
end;

function CreateAnimation(identifier: LongInt;  frames: AnimationTemplate): Animation; overload;
begin
  result := CreateAnimation(identifier, frames, True);
end;

function CreateAnimation(identifier: String;  frames: AnimationTemplate; withSound: Boolean): Animation; overload;
begin
  if frames = nil then exit;
    
  result := CreateAnimation(IndexOf(frames^.animationIds, identifier), frames, withSound);
end;

function CreateAnimation(identifier: String;  frames: AnimationTemplate): Animation; overload;
begin
  result := CreateAnimation(identifier, frames, True);
end;

procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'UpdateAnimation', '');
    try
  {$ENDIF}
  
  if AnimationEnded(anim) then exit;
    
  anim^.frameTime     := anim^.frameTime + pct;
  anim^.enteredFrame  := false;
  
  if anim^.frameTime > anim^.currentFrame^.duration then
  begin
    anim^.frameTime := anim^.frameTime - anim^.currentFrame^.duration; //reduce the time
    anim^.lastFrame := anim^.currentFrame; //store last frame
    anim^.currentFrame := anim^.currentFrame^.next; //get the next frame
    
    if assigned(anim^.currentFrame) and assigned(anim^.currentFrame^.sound) and withSound then
    begin
      PlaySoundEffect(anim^.currentFrame^.sound);
    end;
  end;
  
  {$IFDEF TRACE}
    finally
      TraceExit('sgAnimations', 'UpdateAnimation', '');
    end;
  {$ENDIF}
end;

procedure UpdateAnimation(anim: Animation; pct: Single); overload;
begin
  UpdateAnimation(anim, pct, True);
end;

procedure UpdateAnimation(anim: Animation); overload;
begin
  UpdateAnimation(anim, 1, True);
end;

function AnimationEnded(anim: Animation): Boolean;
begin
  if not Assigned(anim) then
    result := true
  else 
    result := not Assigned(anim^.currentFrame);
end;

function AnimationCurrentCell(anim: Animation): LongInt;
begin
  if not AnimationEnded(anim) then
    result := anim^.currentFrame^.cellIndex
  else if not assigned(anim^.lastFrame) then
    result := -1
  else  //Use the last frame drawn.
    result := anim^.lastFrame^.cellIndex;
end;

function AnimationEnteredFrame(anim: Animation): Boolean;
begin
  if not Assigned(anim) then
    result := false
  else 
    result := anim^.enteredFrame;
end;

function AnimationFrameTime(anim: Animation): Single;
begin
  if not Assigned(anim) then
    result := -1
  else 
    result := anim^.frameTime;
end;


procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: LongInt); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimation', ''); try
  {$ENDIF}
  
  DrawCell(bmp, AnimationCurrentCell(ani), x, y);
  
  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;

procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimation', ''); try
  {$ENDIF}

  DrawCell(bmp, AnimationCurrentCell(ani), pt);

  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;

procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x,y: LongInt); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimation', ''); try
  {$ENDIF}

  DrawCell(dest, bmp, AnimationCurrentCell(ani), x, y);

  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;

procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimation', ''); try
  {$ENDIF}

  DrawCell(dest, bmp, AnimationCurrentCell(ani), pt);

  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;

procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x,y: LongInt); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimationOnScreen', ''); try
  {$ENDIF}
  
  DrawCellOnScreen(bmp, AnimationCurrentCell(ani), x, y);
  
  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;

procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
  {$IFDEF TRACE}
    TraceEnter('sgAnimations', 'DrawAnimationOnScreen', ''); try
  {$ENDIF}
  
  DrawCellOnScreen(bmp, AnimationCurrentCell(ani), pt);
  
  {$IFDEF TRACE}
    finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
  {$ENDIF}
end;


//=============================================================================
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgAnimations', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _Animations := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgAnimations', 'Initialise');
    {$ENDIF}
  end;

end.