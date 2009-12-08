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

unit sgAnimation;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  //----------------------------------------------------------------------------
  // Creating an AnimationFrames
  //----------------------------------------------------------------------------
  
  /// Load animation details from a animation frames file.
  ///
  /// @lib
  /// @sn animationFramesFromFile:%s
  ///
  /// @class AnimationFrames
  /// @constructor
  /// @csn initFromFile:%s
  function LoadAnimationFrames(filename: String) : AnimationFrames;
  
  /// Frees loaded animation frames data. Use this when you will no longer be 
  /// using the animation for any purpose, including within sprites.
  ///
  /// @lib
  ///
  /// @class AnimationFrames
  /// @dispose
  procedure FreeAnimationFrames(var framesToFree: AnimationFrames);
  
  
//----------------------------------------------------------------------------
// AnimationFrames mapping routines
//----------------------------------------------------------------------------
  
  /// Loads and returns a AnimationFrames. The supplied `filename` is used to
  /// locate the AnimationFrames to load. The supplied `name` indicates the 
  /// name to use to refer to this in SwinGame. The `AnimationFrames` can then be
  /// retrieved by passing this `name` to the `FetchAnimationFrames` function. 
  ///
  /// @lib
  ///
  /// @sn animationFramesNamed:%s fromFile:%s
  ///
  /// @class AnimationFrames
  /// @constructor
  /// @csn initWithName:%s forFile:%s
  function MapAnimationFrames(name, filename: String): AnimationFrames;
  
  /// Determines if SwinGame has animation frames loaded for the supplied name.
  /// This checks against all loaded animation frames, those loaded without a name
  /// are assigned the filename as a default.
  ///
  /// @lib
  function HasAnimationFrames(name: String): Boolean;
  
  /// Returns the `AnimationFrames` that has been loaded with the specified name,
  /// see `MapAnimationFrames`.
  ///
  /// @lib
  function FetchAnimationFrames(name: String): AnimationFrames;
  
  /// Releases the SwinGame resources associated with the animation frames of the
  /// specified `name`.
  ///
  /// @lib
  procedure ReleaseAnimationFrames(name: String);
  
  /// Releases all of the bitmaps that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllAnimationFrames();
  
  
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
  function CreateAnimation(identifier: String;  frames: AnimationFrames): Animation;
  
//=============================================================================
implementation
  uses
    SysUtils, Classes, 
    stringhash,         // libsrc
    SDL_Mixer, SDL,     // SDL
    sgShared, sgResources, sgTrace;
//=============================================================================

var
  _Animations: TStringHash;


function LoadAnimationFrames(filename: String) : AnimationFrames;
begin
  
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
