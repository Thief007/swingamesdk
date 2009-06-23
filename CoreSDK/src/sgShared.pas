//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          sgShared.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The shared data and functions that are private to
// the SwinGame SDK.
//
// Change History:
//
// Version 3:
// - 2009-06-05: Andrew:  Created to house all globals
//                        that are hidden from the library

unit sgShared;

interface
  uses SDL, SDL_Image, sgEventProcessing, sgCore, sgTypes;
  
  function ToSDLColor(color: UInt32): TSDL_Color;
  function ToGfxColor(val: Color): Color;
  function NewSDLRect(x, y, w, h: LongInt): SDL_Rect;
  
  procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  
  var
    scr: Bitmap;
    applicationPath: String;
    sdlManager: TSDLManager;
    iconFile: String;
          
    // The base surface is used to get pixel format information for the
    // surfaces created. This is used to create colors etc.
    ///@ignore
    baseSurface: PSDL_Surface;
    
    ErrorMessage: String;
    HasException: Boolean;
    
  const
    DLL_VERSION = 300000;
  
implementation
  uses SysUtils, Math, Classes, sgTrace, SDL_gfx;
  
  function ToSDLColor(color: UInt32): TSDL_Color;
  begin
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      raise Exception.Create('Unable to get color as screen is not created.');
    end;
    
    SDL_GetRGB(color, baseSurface^.format, @result.r, @result.g, @result.b);
  end;
  
  /// Converts the passed in SwinGame color to a Color used by SDL_GFX
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
        
    if w < 0 then
    begin
      result.x := x + w;
      w := -w;
    end
    else result.x := x;

    if h < 0 then
    begin
      result.y := y + h;
      h := -h;
    end
    else result.y := y;

    result.w := Word(w);
    result.h := Word(h);
  end;
  
end.