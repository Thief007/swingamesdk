//=============================================================================
// sgShared.pas
//=============================================================================
//
// The shared data and functions that are private to the SwinGame SDK. This 
// unit should not be accessed directly by games.
//
// Change History:
//
// Version 3:
// - 2009-06-23: Clinton: Comment/format cleanup/tweaks
//                      : Slight optimization to NewSDLRect (redundant code)
//                      : Renamed scr to screen
// - 2009-06-05: Andrew: Created to contain all globals that are hidden from 
//                       the library
//=============================================================================

unit sgShared;

//=============================================================================
interface
//=============================================================================

  uses SDL, SDL_Image, sgEventProcessing, sgCore, sgTypes;
  
  // Takes a 4-byte (32bit) unsigned integer representing colour in the 
  // current SDL pixel format and returns a nice `TSDL_Color` record with 
  // simple red, green and blue components
  function ToSDLColor(color: UInt32): TSDL_Color;
  
  // Converts the passed in SwinGame `Color` format (which changes as does 
  // SDL to whatever the current SDL environment uses) to the Color structure 
  // used by SDL_GFX (which is always 4 bytes in RGBA order).
  function ToGfxColor(val: Color): Color;
  
  // Returns a new `SDL_Rect` at the specified position (``x`` and ``y``) and 
  // size (``w`` and ``h``). SDL_Rect are used internally by SDL to specify 
  // the part of a source or destination image to be used in clipping and 
  // blitting operations.
  function NewSDLRect(x, y, w, h: LongInt): SDL_Rect;
  
  // Used by SwinGame units to register event "handlers" what will be called
  // when SDL events have occured. Events such as mouse movement, button 
  // clicking and key changes (up/down). See sgInput.   
  procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  
  // Global variables that can be shared.
  var
    // This `Bitmap` wraps the an SDL image (and its double-buffered nature) 
    // which is used to contain the current "screen" rendered to the window.   
    screen: Bitmap;
    
    // The full path location of the current executable (or script). This is 
    // particuarly useful when determining the path to resources (images, maps,
    // sounds, music etc). 
    applicationPath: String;
    
    // The singleton instance manager used to check events and called 
    // registered "handlers". See `RegisterEventProcessor`.
    sdlManager: TSDLManager;
    
    // The name of the icon file shown.
    // TODO: Does this work? Full path or just resource/filename?
    iconFile: String;
          
    // This "base" surface is used to get standard pixel format information 
    // for any other surfaces that need to be created and to support colour.
    baseSurface: PSDL_Surface;
    
    // Contains the last error message that has occured if `HasException` is 
    // true. If multiple error messages have occured, only the last is stored.
    // Used only by the generated library code.
    ErrorMessage: String;
    
    // This flag is set to true if an error message has occured. Used only by
    // the generated library code.
    HasException: Boolean;
    
  const
    DLL_VERSION = 300000;
  
//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Math, Classes, sgTrace, SDL_gfx;
  
  function ToSDLColor(color: UInt32): TSDL_Color;
  begin
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      raise Exception.Create('Unable to get color as screen is not created.');
    end;
    
    SDL_GetRGB(color, baseSurface^.format, @result.r, @result.g, @result.b);
  end;
  
  function ToGfxColor(val: Color): Color;
  var
    r, g, b, a: Byte;
  begin
    GetComponents(val, r, g, b, a);
    result := (r shl 24) or (g shl 16) or (b shl 8) or a;
  end;
  
  procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  begin
    sdlManager.RegisterEventProcessor(handle, handle2);
  end;
  
  function NewSDLRect(x, y, w, h: LongInt): SDL_Rect;
  begin
    if (w < 0) or (h < 0) then
      raise Exception.Create('Width and height of a rectangle must be larger than 0');
    // TODO: This can probably be removed now.
    //   
    // if w < 0 then
    // begin
    //   result.x := x + w;
    //   w := -w;
    // end
    // else result.x := x;
    // 
    // if h < 0 then
    // begin
    //   result.y := y + h;
    //   h := -h;
    // end
    // else result.y := y;
    result.x := x;
    result.y := y;
    result.w := Word(w);
    result.h := Word(h);
  end;
  
initialization
begin
  sdlManager := TSDLManager.Create();
end;

finalization
  if sdlManager <> nil then
  begin
    sdlManager.Free();
    sdlManager := nil;
  end;

end.