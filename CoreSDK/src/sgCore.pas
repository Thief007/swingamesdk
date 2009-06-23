//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          sgCore.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Core unit contains the main SwinGame routines and
// data types. These will be required by any game using
// the API.
//
// Change History:
//
// Version 3:
// - 2009-06-23: Andrew: Added getting and setting of AppPath
// - 2009-06-05: Andrew: Moved out shared data/functions
// - 2009-06-04: Andrew:  Started on processing comments.
//
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-10: Andrew: Added Matrix type to Core.
//                       Removed W from Vector
//                       Added Rotate and Zoom to Sprite
//             Moved to manual double buffer
//             Added matrix to allow matrix manipulation in Shapes package
// - 2008-12-09: Andrew: Added in correction to fix issue with FPC 2.2.2 on Mac.
//
// Version 1.1.6:
// - 2008-05-09: Andrew: Added tracing for OpenGraphicsWindow
// Version 1.1:
// - 2008-04-02: Andrew: Added handling of other resources
// - 2008-03-09: Andrew: Added extra exception handling
// - 2008-02-16: Andrew: Added Mac boot support, and 
//                       Fixed scr bitmap to have width and height
// - 2008-01-28: Andrew: Fixed Toggle issue.
// - 2008-01-27: Andrew: Implemented the Change of Screen size fix..
//      Found a different bug which means that the changes
//      were not needed. Need to test in windows
// - 2008-01-21: Aki: Implemented timer
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

/// SwinGame's Core is responsible for core functionality such as creating
/// the game window, coordinating event processing, and refreshing the
/// screen. Core also contains components that can be used to create and
/// manage timers, sleep during execution, and other time related management.
///
/// @module Core
/// @static
unit sgCore;

{$IFDEF UNIX}
  {$linklib gcc}
{$ENDIF}

interface  
  {$I sgTrace.inc}
  
  uses sgTypes, SDL;
  
  /// @lib ProcessEvents
  procedure ProcessEvents();
  
  /// @lib WindowCloseRequested
  /// @uname WindowCloseRequested
  function WindowCloseRequested(): Boolean;
  
  /// @lib SetIcon
  procedure SetIcon(iconFilename: String);
  
  /// @lib OpenGraphicsWindow
  /// @uname OpenGraphicsWindow
  procedure OpenGraphicsWindow(caption: String; width, height: LongInt); overload;
  
  /// @lib OpenGraphicsWindow(caption, 800, 600)
  /// @uname OpenGraphicsWindow800x600
  procedure OpenGraphicsWindow(caption : String); overload;
  
  /// @lib ChangeScreenSize
  procedure ChangeScreenSize(width, height: LongInt);
  
  /// @lib ToggleFullScreen
  procedure ToggleFullScreen();
  
  /// @lib RefreshScreen
  procedure RefreshScreen(); overload;  
  
  /// @lib RefreshScreenRestrictFPS
  /// @uname RefreshScreenRestrictFPS 
  procedure RefreshScreen(TargetFPS : LongInt); overload;
  
  /// @lib TakeScreenshot
  procedure TakeScreenshot(basename: String);
  
  /// @lib ScreenWidth
  function  ScreenWidth(): LongInt;
  
  /// @lib ScreenHeight
  function  ScreenHeight(): LongInt;
  
  /// @lib GetColorForBitmap
  /// @uname GetColorForBitmap
  function GetColor(forBitmap: Bitmap; apiColor: Color): Color; overload;
  
  /// @lib GetColor
  function GetColor(red, green, blue, alpha: Byte) : Color; overload;  
  
  /// @lib GetColor(red, green, blue, 255)
  /// @uname GetColorRGB
  function GetColor(red, green, blue : Byte) : Color; overload;
  
  /// @lib GetComponents
  procedure GetComponents(color: Color; out r, g, b, a: byte);
  
  /// @lib GetRGBFloatColor
  function GetRGBFloatColor(r,g,b: Single): Color;
  
  /// @lib GetHSBColor
  function GetHSBColor(hue, saturation, brightness: Single): Color;
  
  /// @lib GetFramerate
  function GetFramerate(): LongInt;
  
  /// @lib GetTicks
  function GetTicks(): UInt32;
  
  /// @lib Sleep
  procedure Sleep(time : UInt32);
  
  /// @lib GetPathToResource
  function GetPathToResource(filename: String; kind: ResourceKind): String; overload;
  
  /// @lib GetPathToOtherResource
  /// @uname GetPathToOtherResource
  function GetPathToResource(filename: String): String; overload;
  
  /// @lib Cos
  function Cos(angle: Single): Single;
  
  /// @lib Sin
  function Sin(angle: Single): Single;
  
  /// @lib Tan
  function Tan(angle: Single): Single;
  
  /// @lib CreateTimer
  function CreateTimer() : Timer;
  
  /// @lib FreeTimer
  procedure FreeTimer(var toFree: Timer);
  
  /// @lib StartTimer
  procedure StartTimer(toStart : Timer);
  
  /// @lib StopTimer
  procedure StopTimer(toStop : Timer);
  
  /// @lib PauseTimer
  procedure PauseTimer(toPause : Timer);
  
  /// @lib UnpauseTimer
  procedure UnpauseTimer(toUnpause : Timer);
  
  /// @lib GetTimerTicks
  function GetTimerTicks(toGet : Timer) : UInt32;
  
  /// @lib GetTransparency
  function GetTransparency(color: Color): byte;
  
  /// @lib GetRed
  function GetRed(color: Color): byte;
  
  /// @lib GetGreen
  function GetGreen(color: Color): byte;
  
  /// @lib GetBlue
  function GetBlue(color: Color): byte;
  
  /// @lib CalculateFramerate
  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);  
  
  /// @lib GetPathToResourceWithBaseAndKind
  /// @uname GetPathToResourceWithBase
  function GetPathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload;
  
  /// @lib GetPathToOtherResourceWithBase
  /// @uname GetPathToOtherResourceWithBase
  function GetPathToResourceWithBase(path, filename: String) : String; overload;
  
  /// @lib DLLVersion
  function DLLVersion(): LongInt;
  
  /// @lib GetExceptionMessage
  function GetExceptionMessage(): String;
  
  /// @lib HasExceptionOccured
  function HasExceptionOccured(): Boolean;
  
  /// @lib
  procedure SetAppPath(path: String; withExe: Boolean);
  /// @lib
  function AppPath(): String;
  
  var
    //Preset colours, do not change these values.
    ColorBlue, ColorGreen, ColorRed, ColorWhite,
    ColorBlack, ColorYellow, ColorPink, ColorTurquoise,
    ColorGrey, ColorMagenta, ColorTransparent,
    ColorLightGrey: Color;
  
implementation
  uses SysUtils, Math, Classes, sgTrace, SDL_gfx, sgShared, sgEventProcessing, SDL_Image;
  
  
  function DLLVersion(): LongInt;
  begin
    result := DLL_VERSION;
  end;
  
  function GetExceptionMessage(): String;
  begin
    result := ErrorMessage;
  end;
  
  function HasExceptionOccured(): Boolean;
  begin
    result := HasException;
  end;
  
  
  /// Record: FPSCalcInfo
  ///
  /// This record contains details required for the
  /// Frames per second calculations.
  type FPSCalcInfo = record
    valuesArray : Array [0..59] of UInt32;
    arrayIndex, loopCount : LongInt;
    high, low, average : Single;
  end;
  
  var
    // Timing details related to calculating FPS
    lastDrawUpdateTime: UInt32;
    trueScr: PSDL_Surface;
    renderFPSInfo: FPSCalcInfo;
  
  /// ProcessEvents allows the SwinGame API to react to user interactions. This
  /// routine checks the current keyboard and mouse states. This routine must
  /// be called frequently within your game loop to enable user interaction.
  ///
  /// Side Effects
  /// - Reads user interaction events
  /// - Updates keys down, text input, etc.
  procedure ProcessEvents();
  var
    x, y: LongInt;
  begin
    SDL_GetRelativeMouseState(x, y);
    sdlManager.ProcessEvents();
  end;
  
  function Cos(angle: Single): Single;
  begin
    result := System.Cos(DegToRad(angle));
  end;

  function Sin(angle: Single): Single;
  begin
    result := System.Sin(DegToRad(angle));
  end;

  function Tan(angle: Single): Single;
  begin
    result := Math.Tan(DegToRad(angle));
  end;
  
  procedure SetupScr();
  begin
    if scr = nil then New(scr)
    else if (scr.surface <> nil) then SDL_FreeSurface(scr.surface);
    
    with trueScr.format^ do
    begin
      scr.surface := SDL_CreateRGBSurface(SDL_HWSURFACE, ScreenWidth(), ScreenHeight(), 32, 
                                  RMask, GMask, BMask, SDL_Swap32($000000FF));

      //Turn off alpha blending for when scr is blit onto trueScr
      SDL_SetAlpha(scr.surface, 0, 255);
      SDL_FillRect(scr.surface, @scr.surface.clip_rect, ColorLightGrey);
                
      baseSurface := scr.surface;
      
      scr.width := trueScr.w;
      scr.height := trueScr.h;
    end;
  end;
  
  /// Sets up the graphical window for the specified width and height.
  /// Sets the caption of the window, and the icon if one is specified.
  procedure InitSDL(caption: String; screenWidth, screenHeight: LongInt);
  var
    icon: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCore', 'InitSDL');
    {$ENDIF}

    if (screenWidth < 1) or (screenHeight < 1) then raise Exception.Create('Screen Width and Height must be greater then 0 when opening a Graphical Window');
  
    if Length(iconFile) > 0 then
    begin
      try
        icon := IMG_Load(PChar(iconFile));
        SDL_WM_SetIcon(icon, 0);
        SDL_FreeSurface(icon);
      except
        raise Exception.Create('The icon file specified could not be loaded');
      end;
    end;
    
    trueScr := SDL_SetVideoMode(screenWidth, screenHeight, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if trueScr = nil then raise Exception.Create('Unable to create window drawing surface... ' + SDL_GetError());

    SetupScr();
    SDL_WM_SetCaption(PChar(caption), nil);

    {$IFDEF TRACE}
      TraceExit('sgCore', 'InitSDL');
    {$ENDIF}
  end;
  
  procedure ClearArray(var theArray: Array of UInt32);
  var
    index : LongInt;
  begin
    for index := Low(theArray) to High(theArray) do
    begin
      theArray[index] := 0;
    end;
  end;
  
  //Used to initialise the Frame Per Second structure.
  procedure InitFPSCalcInfo(var fpsInfo : FPSCalcInfo);
  begin
    ClearArray(fpsInfo.valuesArray);
    
    fpsInfo.arrayIndex := 0;
    fpsInfo.loopCount := 0;
    fpsInfo.High := 1;
    fpsInfo.Low := 1;
    fpsInfo.Average := 1000000;
  end;
  
  function GetRunningAverage(var runningArray : Array of UInt32; newNumber : UInt32; var index : LongInt): Single;
  var
    loopCount : LongInt;
    sum : Double;
  begin
    sum := 0;
  
    runningArray[index] := newNumber;
    for loopCount := Low(runningArray) to High(runningArray) do
    begin
      sum := sum + runningArray[loopCount];
    end;
  
    result := sum / Length(runningArray);
  
    //Inc index
    index := index + 1;
    if index > High(runningArray) then
      index := Low(runningArray);
  end;
  
  procedure DoFPSCalculations(var fpsInfo : FPSCalcInfo; nowTime, lastUpdateTime : UInt32);
  begin
    //Populate the running average for the first 10 calcs
    if fpsInfo.loopCount < 10 then
    begin
      //Don't get the avg, because this may result in a 
        //div 0 error if avg is 0 (when calcing 1000/avg)
      GetRunningAverage(fpsInfo.valuesArray, nowTime - lastUpdateTime, 
                         fpsInfo.arrayIndex);
      fpsInfo.loopCount := fpsInfo.loopCount + 1;
    end
    else //All other calcs get the average as normal
      fpsInfo.average := GetRunningAverage(fpsInfo.valuesArray, 
                            nowTime - lastUpdateTime, fpsInfo.arrayIndex);
  
    //On the 10th calcs set the min maxes
    if fpsInfo.loopCount = 10 then
    begin
      //Get the running average
      fpsInfo.average := GetRunningAverage(fpsInfo.valuesArray,
                         nowTime - lastUpdateTime,
                         fpsInfo.arrayIndex);;
      fpsInfo.High := fpsInfo.average;
      fpsInfo.Low := fpsInfo.average;
      fpsInfo.loopCount := fpsInfo.loopCount + 1;
    end;
  
    if fpsInfo.average = 0.0 then
        fpsInfo.average := 0.01;
    
    //Adjust the min/maxes
    if fpsInfo.average > fpsInfo.High then fpsInfo.High := fpsInfo.average;
    if fpsInfo.average < fpsInfo.Low then fpsInfo.Low := fpsInfo.average;
  end;
  
  /// Sets the icon for the window. This must be called before openning the
  /// graphics window. The icon is loaded as a bitmap, though this can be from
  /// any kind of bitmap file.
  ///
  /// @param iconFilename:   The name of the file to load as the images icon
  ///
  ///Side Effects
  /// - The icon will be loaded and used as the windows icon when the window
  /// is opened.
  procedure SetIcon(iconFilename: String);
  begin
    iconFile := iconFilename;
  end;
  
    /// Switches the application to full screen or back from full screen to
  /// windowed.
  ///
  /// Side Effects:
  /// - The window switched between fullscreen and windowed
  procedure ToggleFullScreen();
  var
    oldScr: PSDL_Surface;
  begin
    oldScr := trueScr;
    
    try
      trueScr := SDL_SetVideoMode(oldScr.w, oldScr.h, 32, oldScr.flags xor SDL_FULLSCREEN);
      //Remember... trueScr is a pointer to screen buffer, not a "surface"!
    except on exc: Exception do
    end;
  end;
  
  /// Changes the size of the screen.
  ///
  /// @param width, height: The new width and height of the screen
  ///
  /// Side Effects:
  /// - The screen changes to the specified size
  procedure ChangeScreenSize(width, height: LongInt);
  var
    oldScr: PSDL_Surface;
    //toggle: Boolean;
  begin
    if (scr = nil) then
      raise Exception.Create('Screen has not been created. Unable to get screen width.');
        
    if (width < 1) or (height < 1) then 
      raise Exception.Create('Screen Width and Height must be greater then 0 when resizing a Graphical Window');
    
    if (width = ScreenWidth()) and (height = ScreenHeight()) then exit;
    
    oldScr := trueScr;
    trueScr := SDL_SetVideoMode(width, height, 32, oldScr.flags);
    SetupScr();
  end;
  
  /// Returns the width of the screen currently displayed.
  ///
  /// @returns: The screen's width
  function ScreenWidth(): LongInt;
  begin
      if (trueScr = nil) then
          raise Exception.Create('Screen has not been created. Unable to get screen width.');

    result := trueScr.w;
  end;

  /// Returns the height of the screen currently displayed.
  ///
  /// @returns: The screen's height
  function ScreenHeight(): LongInt;
  begin
      if (trueScr = nil) then
          raise Exception.Create('Screen has not been created. Unable to get screen width.');

    result := trueScr.h;
  end;

  {$ifdef DARWIN}
    {$linklib libobjc.dylib}
    
   procedure NSApplicationLoad(); cdecl; external 'Cocoa'; {$EXTERNALSYM NSApplicationLoad}
   function objc_getClass(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_getClass}
   function sel_registerName(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM sel_registerName}
   function objc_msgSend(self, cmd: LongInt): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_msgSend}

  {$endif}    
  
  /// Opens the graphical window so that it can be drawn onto. You can set the
  /// icon for this window using SetIcon. The window itself is only drawn when
  /// you call RefreshScreen. All windows are opened at 32 bits per pixel. You
  /// can toggle fullscreen using ToggleFullScreen. The window is closed when
  /// the application terminates.
  ///
  /// @params caption: The caption for the window
  /// @params width:   The width of the window
  /// @params height: The height of the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  procedure OpenGraphicsWindow(caption : String; width : LongInt; height : LongInt); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCore', 'OpenGraphicsWindow', caption + ': W' + IntToStr(width) + ': H' + IntToStr(height));
    {$ENDIF}
    
    if scr <> nil then
          raise Exception.Create('Screen has been created. Cannot create multiple windows.');
      
    try         
      InitSDL(caption, width, height);
      InitFPSCalcInfo(renderFPSInfo);
  
      //Init the colors
      ColorWhite := GetColor(255, 255, 255, 255);
      ColorGreen := GetColor(0, 255, 0, 255);
      ColorBlue := GetColor(0, 0, 255, 255);
      ColorBlack := GetColor(0, 0, 0, 255);
      ColorRed := GetColor(255, 0, 0, 255);
      ColorYellow := GetColor(255, 255, 0, 255);
      ColorPink := GetColor(255, 20, 147, 255);
      ColorTurquoise := GetColor(0, 206, 209, 255);
      ColorGrey := GetColor(128, 128, 128, 255);
      ColorMagenta := GetColor(255, 0, 255, 255);
      ColorTransparent := GetColor(0, 0, 0, 0);
      ColorLightGrey := GetColor(200, 200, 200, 255);
      
      SDL_FillRect(scr.surface, nil, ColorGrey);
      stringColor(scr.surface, screenWidth div 2 - 30, screenHeight div 2, PChar('Loading ...'), ToGFXColor(ColorWhite));
      RefreshScreen();
    except on e: Exception do raise Exception.Create('Error in OpenGraphicsWindow: ' + e.Message);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgCore', 'OpenGraphicsWindow');
    {$ENDIF}
  end;
  
  /// Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
  /// for more options.
  /// @params caption: The caption for the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  procedure OpenGraphicsWindow(caption : String); overload;
  begin
    OpenGraphicsWindow(caption, 800,600);
  end;
  
  procedure RefreshScreen(); overload;
  var
    nowTime: UInt32;
  begin       
    //Draw then delay
    sdlManager.DrawCollectedText(scr.surface);    
    SDL_BlitSurface(scr.surface, nil, trueScr, nil);
    SDL_Flip(trueScr);
    
    nowTime := GetTicks();
    DoFPSCalculations(renderFPSInfo, nowTime, lastDrawUpdateTime);
    lastDrawUpdateTime := nowTime;  
  end;
  
  /// Draws the current drawing to the screen. This must be called to display
  /// anything to the screen. This will draw all drawing operations, as well
  /// as the text being entered by the user.
  ///
  /// Side Effects:
  /// - The current drawing is shown on the screen.
  procedure RefreshScreen(TargetFPS : LongInt); overload;
  var
    nowTime: UInt32;
    difference : UInt32;
  begin
    sdlManager.DrawCollectedText(scr.surface);    
    SDL_BlitSurface(scr.surface, nil, trueScr, nil);
    SDL_Flip(trueScr);
    
    nowTime := GetTicks();
    difference := nowTime - lastDrawUpdateTime;
    
    while (difference < ((1 / TargetFPS) * 1000)) do
    begin
      Sleep(1);
      nowTime := GetTicks();
      difference := nowTime - lastDrawUpdateTime;
    end;  
    
    DoFPSCalculations(renderFPSInfo, nowTime, lastDrawUpdateTime);
    lastDrawUpdateTime := nowTime;    
  end;
  
  /// Saves the current screen a bitmap file. The file will be saved into the
  /// current directory.
  ///
  /// @params basename   The base name for the screen shot. e.g. "GemCollector"
  ///
  /// Side Effects:
  /// - Saves the current screen image to a bitmap file.
  procedure TakeScreenShot(basename: String);
  var
    filename: String;
    i : LongInt;
  begin
    filename := basename + '.bmp';
    i := 1;
    
    while FileExists(filename) do
    begin
      filename := basename + IntToStr(i) + '.bmp';
      i := i + 1;
    end;
    
    if SDL_SaveBMP(scr.surface, PChar(filename)) = -1 then
    begin
     raise Exception.Create('Failed to save ' + basename + '.bmp : ' + SDL_GetError());
    end;
  end;
  
  procedure GetComponents(color: Color; out r, g, b, a: byte);
  begin
    if baseSurface = nil then raise Exception.Create('Cannot read screen format. Ensure window is open.');
      
    SDL_GetRGBA(color, baseSurface.Format, @r, @g, @b, @a);
  end;
  
  function GetTransparency(color: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    GetComponents(color, r, g, b, a);
    result := a;
  end;
  
  function GetRed(color: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    GetComponents(color, r, g, b, a);
    result := r;
  end;
  
  function GetGreen(color: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    GetComponents(color, r, g, b, a);
    result := g;
  end;
  
  function GetBlue(color: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    GetComponents(color, r, g, b, a);
    result := b;
  end;
  
  /// Maps a color from a given bitmap. This is used when determining color
  /// keys for transparent images.
  ///
  /// @param forBitmap:   the bitmap to get the color for
  /// @param apiColor:     The api color to match
  /// @returns:           The color matched to the bitmaps pixel format
  function GetColor(forBitmap: Bitmap; apiColor: Color): Color; overload;
  var
    temp: TSDL_Color;
  begin
    if (forBitmap = nil)
      or (forBitmap.surface = nil)
      or (forBitmap.surface.format = nil) then
    begin
      raise Exception.Create('Unable to get color as bitmap not specified');
    end;

    temp := ToSDLColor(apiColor);
    result := SDL_MapRGB(forBitmap.surface.format, temp.r, temp.g, temp.b);
  end;
  
  /// Gets a color given its RGBA components.
  ///
  /// @param red, green, blue, alpha:  Components of the color
  /// @returns: The matching colour
  function GetColor(red, green, blue, alpha: Byte) : Color; overload;
  begin
    if (baseSurface = nil) or (baseSurface.format = nil) then
      raise Exception.Create('Unable to GetColor as the window is not open');

    try
      result := SDL_MapRGBA(baseSurface.format, red, green, blue, alpha);
    except
      raise Exception.Create('Error occured while trying to get a color from RGBA components');
    end;
  end;
  
  /// Gets a color given its RGB components.
  ///
  /// @param red, green, blue:   Components of the color
  /// @returns:                 The matching colour
  function GetColor(red, green, blue : Byte) : Color;
  begin
    result := GetColor(red, green, blue, 255);
  end;
  
  /// Returns a color from a floating point RBG value set.
  ///
  /// @param r,g,b: Components for color 0 = none 1 = full
  function GetRGBFloatColor(r,g,b: Single): Color;
  begin
    result := GetColor(Round(r * 255), Round(g * 255), Round(b * 255));
  end;

  /// Returs a color from the HSB input.
  ///
  /// @param hue, saturation, brightness: Values between 0 and 1
  /// @returns The matching color
  function GetHSBColor(hue, saturation, brightness: Single): Color;
  var
    domainOffset: Single;
    red, green, blue: Single;
  begin
    if brightness = 0 then
    begin
      result := ColorBlack;
      exit;
    end;
    
    if saturation = 0 then
    begin
      result := GetRGBFloatColor(brightness, brightness, brightness);
      exit;
    end;
    
    if hue < 1.0 / 6 then
    begin
      //Red domain... green ascends
      domainOffset := hue;
      red   := brightness;
      blue    := brightness * (1.0 - saturation);
          green   := blue + (brightness - blue) * domainOffset * 6;
    end
    else if hue < 2.0 / 6 then
    begin
      // yellow domain; red descends
          domainOffset := hue - 1.0 / 6;
          green := brightness;
          blue  := brightness * (1.0 - saturation);
          red   := green - (brightness - blue) * domainOffset * 6;
    end
    else if hue < 3.0 / 6 then
        begin
          // green domain; blue ascends
          domainOffset := hue - 2.0 / 6;
          green := brightness;
          red   := brightness * (1.0 - saturation);
          blue  := red + (brightness - red) * domainOffset * 6;
        end
      else if hue < 4.0 / 6 then
        begin
          // cyan domain; green descends
          domainOffset := hue - 3.0 / 6;
      blue  := brightness;
          red   := brightness * (1.0 - saturation);
          green := blue - (brightness - red) * domainOffset * 6;
      end
        else if hue < 5.0 / 6 then
       begin
          // blue domain; red ascends
          domainOffset := hue - 4.0 / 6;
          blue  := brightness;
          green := brightness * (1.0 - saturation);
          red   := green + (brightness - green) * domainOffset * 6;
        end
      else
      begin
    // magenta domain; blue descends
    domainOffset := hue - 5.0 / 6;
    red   := brightness;
    green := brightness * (1.0 - saturation);
    blue  := red - (brightness - green) * domainOffset * 6;
      end;
      
      result := GetRGBFloatColor(red, green, blue);
  end;
  
  /// Puts the process to sleep for a specified number of
  /// milliseconds. This can be used to add delays into your
  /// game.
  ///
  /// @param time - The number of milliseconds to sleep
  ///
  /// Side Effects
  /// - Delay before returning
  procedure Sleep(time : UInt32);
  begin
    SDL_Delay(time);
  end;
  
  /// Checks to see if the window has been asked to close. You need to handle
  /// this if you want the game to end when the window is closed. This value
  /// is updated by the ProcessEvents routine.
  ///
  /// @returns : True if the window has been requested to close.
  function WindowCloseRequested(): Boolean;
  begin
    if scr = nil then 
      result := false
    else
      result := sdlManager.HasQuit();
  end;
  
  /// Gets the number of milliseconds that have passed. This can be used to
  /// determine timing operations, such as updating the game elements.
  ///
  /// @returns     The number of milliseconds passed
  function GetTicks(): UInt32;
  begin
    result := SDL_GetTicks();
  end;
  
  /// Returns the average framerate for the last 10 frames as an integer.
  ///
  /// @returns     The current average framerate
  function GetFramerate(): LongInt;
  begin
    if renderFPSInfo.average = 0 then
      result := 60
    else
      result := Round(1000 / renderFPSInfo.average);
  end;
  
  function GetPathToResourceWithBase(path, filename: String; kind: ResourceKind) : String; overload;
  begin
    case kind of
    {$ifdef UNIX}
      FontResource: result := GetPathToResourceWithBase(path, 'fonts/' + filename);
      SoundResource: result := GetPathToResourceWithBase(path, 'sounds/' + filename);
      BitmapResource: result := GetPathToResourceWithBase(path, 'images/' + filename);
      MapResource: result := GetPathToResourceWithBase(path, 'maps/' + filename);
    {$else}
      FontResource: result := GetPathToResourceWithBase(path, 'fonts\' + filename);
      SoundResource: result := GetPathToResourceWithBase(path, 'sounds\' + filename);
      BitmapResource: result := GetPathToResourceWithBase(path, 'images\' + filename);
      MapResource: result := GetPathToResourceWithBase(path, 'maps\' + filename);
    {$endif}

      else result := GetPathToResourceWithBase(path, filename);
    end;
  end;

  function GetPathToResourceWithBase(path, filename: String) : String; overload;
  begin
    {$ifdef UNIX}
      {$ifdef DARWIN}
        result := path + '/../Resources/';
      {$else}
        result := path + '/Resources/';
      {$endif}
    {$else}
    //Windows
      result := path + '\resources\';
    {$endif}
    result := result + filename;
  end;
  
  function GetPathToResource(filename: String): String; overload;
  begin
    result := GetPathToResourceWithBase(applicationPath, filename);
  end;

  function GetPathToResource(filename: String; kind: ResourceKind) : String; overload;
  begin 
    result := GetPathToResourceWithBase(applicationPath, filename, kind);
  end;
  
  function CreateTimer() : Timer;
  begin
    New(result);
    with result^ do
    begin
      startTicks := 0;
      pausedTicks := 0;
      paused := false;
      started := false;
    end;
  end;
  
  procedure FreeTimer(var toFree: Timer);
  begin
    if Assigned(toFree) then Dispose(toFree);
    toFree := nil;
  end;
  
  procedure StartTimer(toStart : Timer);
  begin
    if not Assigned(toStart) then raise Exception.Create('No timer supplied');
    with toStart^ do
    begin
      started := true;
      paused := false;
      startTicks := SDL_GetTicks();
    end;
  end;
  
  procedure StopTimer(toStop : Timer);
  begin
    if not Assigned(toStop) then raise Exception.Create('No timer supplied');
    with toStop^ do
    begin
      started := false;
      paused := false;
    end;
  end;
  
  procedure PauseTimer(toPause : Timer);
  begin
    if not Assigned(toPause) then raise Exception.Create('No timer supplied');
    with toPause^ do
    begin
      if started and (not paused) then
      begin
        paused := true;
        pausedTicks := SDL_GetTicks() - startTicks;
      end;
    end;
  end;
  
  procedure UnpauseTimer(toUnpause : Timer);
  begin
    if not Assigned(toUnpause) then raise Exception.Create('No timer supplied');
    with toUnpause^ do
    begin
      if paused then
      begin
        paused := false;
        startTicks := SDL_GetTicks() - pausedTicks;
        pausedTicks := 0;
      end;
    end;
  end;
  
  function GetTimerTicks(toGet : Timer) : UInt32;
  begin
    if not Assigned(toGet) then raise Exception.Create('No timer supplied');
          
    with toGet^ do
    begin
      if started then
      begin
        if paused then result := pausedTicks
        else result := SDL_GetTicks() - startTicks;
        exit;
      end;
    end;
    result := 0;
  end;
  
  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);
  var
    avg, hi, lo: Single;
  begin
    if renderFPSInfo.average = 0 then
      avg := 9999
    else
      avg := (1000 / renderFPSInfo.average);
    
    lo := (1000 / renderFPSInfo.high);
    hi := (1000 / renderFPSInfo.low);

    Str(avg:4:1, average);
    Str(hi:4:1, highest);
    Str(lo:4:1, lowest);   

    if avg < 10 then
      textColor := ColorRed
    else if avg < 50 then
      textColor := ColorYellow
    else
      textColor := ColorGreen;
  end;
  
  procedure SetAppPath(path: String; withExe: Boolean);
  begin
    if withExe then applicationPath := ExtractFileDir(path)
    else applicationPath := path;
  end;
  
  function AppPath(): String;
  begin
    result := applicationPath;
  end;

{$ifdef DARWIN}
var
  NSAutoreleasePool: LongInt;
  pool: LongInt;
{$endif}

    
initialization
begin
  {$IFDEF TRACE}
    TraceEnter('sgCore', 'initialization');
  {$ENDIF}
  
  {$ifdef DARWIN}
    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgCore', 'Info', 'initialization', 'Loading Mac version');
    {$ENDIF}

    //FIX: Error with Mac and FPC 2.2.2
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    
    NSAutoreleasePool := objc_getClass('NSAutoreleasePool');
    pool := objc_msgSend(NSAutoreleasePool, sel_registerName('new'));

    objc_msgSend(pool, sel_registerName('init'));
    NSApplicationLoad();
   {$endif}
  
  if SDL_Init(SDL_INIT_EVERYTHING) = -1 then
  begin
    {$IFDEF Trace}
    TraceIf(tlError, 'sgCore', 'Error Loading SDL', 'initialization', SDL_GetError());
    {$ENDIF}
    raise Exception.Create('Error loading sdl... ' + SDL_GetError());
  end;
    
  //Unicode required by input manager.
  SDL_EnableUNICODE(SDL_ENABLE);

  sdlManager := TSDLManager.Create();
  try
    if ParamCount() >= 0 then
      applicationPath := ExtractFileDir(ParamStr(0))
    else
      applicationPath := '';
  except
    applicationPath := '';
  end;

  scr := nil;
  baseSurface := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgCore', 'initialization');
  {$ENDIF}
end;

finalization
begin
  {$IFDEF TRACE}
    TraceEnter('sgCore', 'finalization');
  {$ENDIF}

  if sdlManager <> nil then
  begin
    sdlManager.Free();
    sdlManager := nil;
  end;

  if scr <> nil then
  begin
    if scr.surface <> nil then
      SDL_FreeSurface(scr.surface);
    
    Dispose(scr);
    scr := nil;
    //scr and baseSurface are now the same!
    baseSurface := nil;
  end;
  
  SDL_Quit();
  
  {$IFDEF TRACE}
    TraceExit('sgCore', 'finalization');
  {$ENDIF}
end;

end.