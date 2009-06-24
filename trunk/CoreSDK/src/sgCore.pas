//----------------------------------------------------------------------------
// sgCore.pas
//----------------------------------------------------------------------------
//
// The Core unit contains the main SwinGame routines and data types. These will
// be required by any game using the API.
//
// Change History:
//
// Version 3:
// - 2009-06-23: Clinton: Initial format/comment cleanup...
//                      : Reordered / grouped all methods
//                      : Removed private ClearArray (only one line)
//                      : Renamed private _fpsData, _screen and _lastUpdateTime
//                      : Renamed private _InitFPSData and removed params
//                      : Renamed FPSCalcInfo to FPSData, renamed internals
//
// - 2009-06-23: Andrew: Added getting and setting of AppPath
// - 2009-06-05: Andrew: Moved out shared data/functions
// - 2009-06-04: Andrew: Started on processing comments.
//
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-10: Andrew: Added Matrix type to Core.
//                       Removed W from Vector
//                       Added Rotate and Zoom to Sprite
//                       Moved to manual double buffer
//                       Added matrix to allow matrix manipulation in Shapes package
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
//                       Found a different bug which means that the changes
//                       were not needed. Need to test in windows
// - 2008-01-21: Aki: Implemented timer
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various
//----------------------------------------------------------------------------

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

//----------------------------------------------------------------------------
interface
//----------------------------------------------------------------------------

  {$I sgTrace.inc}

  uses sgTypes, SDL;

  //----------------------------------------------------------------------------
  // Library Version
  //----------------------------------------------------------------------------

  /// @lib
  function DLLVersion(): LongInt;

  //----------------------------------------------------------------------------
  // Exception Notification/Message
  //----------------------------------------------------------------------------

  /// @lib
  function GetExceptionMessage(): String;

  /// @lib
  function HasExceptionOccured(): Boolean;


  //----------------------------------------------------------------------------
  // Icon / Window Open / Screen Size / Resize
  //----------------------------------------------------------------------------

  /// Sets the icon for the window. This must be called before openning the
  /// graphics window. The icon is loaded as a bitmap, though this can be from
  /// any kind of bitmap file.
  ///
  /// @param filename The name of the file to load as the images icon
  ///
  ///Side Effects
  /// - The icon will be loaded and used as the windows icon when the window
  /// is opened.
  ///
  /// @lib
  procedure SetIcon(filename: String);

  /// Opens the graphical window so that it can be drawn onto. You can set the
  /// icon for this window using SetIcon. The window itself is only drawn when
  /// you call RefreshScreen. All windows are opened at 32 bits per pixel. You
  /// can toggle fullscreen using ToggleFullScreen. The window is closed when
  /// the application terminates.
  ///
  /// @param caption The caption for the window
  /// @param width The width of the window
  /// @param height The height of the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib
  /// @uname OpenGraphicsWindow
  procedure OpenGraphicsWindow(caption: String; width, height: LongInt); overload;

  /// Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
  /// for more options.
  /// @param caption: The caption for the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib OpenGraphicsWindow(caption, 800, 600)
  /// @uname OpenGraphicsWindow800x600
  procedure OpenGraphicsWindow(caption: String); overload;

  /// Changes the size of the screen.
  ///
  /// @param width, height: The new width and height of the screen
  ///
  /// Side Effects:
  /// - The screen changes to the specified size
  ///
  /// @lib
  procedure ChangeScreenSize(width, height: LongInt);

  /// Switches the application to full screen or back from full screen to
  /// windowed.
  ///
  /// Side Effects:
  /// - The window switched between fullscreen and windowed
  ///
  /// @lib
  procedure ToggleFullScreen();

  /// Returns the width of the screen currently displayed.
  ///
  /// @returns: The screen's width
  ///
  /// @lib
  function ScreenWidth(): LongInt;

  /// Returns the height of the screen currently displayed.
  ///
  /// @returns: The screen's height
  ///
  /// @lib
  function ScreenHeight(): LongInt;

  /// Saves the current screen a bitmap file. The file will be saved into the
  /// current directory.
  ///
  /// @param basename   The base name for the screen shot. e.g. "GemCollector"
  ///
  /// Side Effects:
  /// - Saves the current screen image to a bitmap file.
  ///
  /// @lib TakeScreenshot
  procedure TakeScreenshot(basename: String);

  //----------------------------------------------------------------------------
  // Game-loop Essentials: WindowOpen, ProcessEvents, WindowClose
  //----------------------------------------------------------------------------

  /// Checks to see if the window has been asked to close. You need to handle
  /// this if you want the game to end when the window is closed. This value
  /// is updated by the ProcessEvents routine.
  ///
  /// @returns: True if the window has been requested to close.
  ///
  /// @lib WindowCloseRequested
  /// @uname WindowCloseRequested
  function WindowCloseRequested(): Boolean;

  /// ProcessEvents allows the SwinGame API to react to user interactions. This
  /// routine checks the current keyboard and mouse states. This routine must
  /// be called frequently within your game loop to enable user interaction.
  ///
  /// Side Effects
  /// - Reads user interaction events
  /// - Updates keys down, text input, etc.
  ///
  /// @lib ProcessEvents
  procedure ProcessEvents();



  /// Draws the current drawing to the screen. This must be called to display
  /// anything to the screen. This will draw all drawing operations, as well
  /// as the text being entered by the user.
  ///
  /// Side Effects:
  /// - The current drawing is shown on the screen.
  ///
  /// @lib RefreshScreen
  procedure RefreshScreen(); overload;

  /// @lib RefreshScreenRestrictFPS
  /// @uname RefreshScreenRestrictFPS
  procedure RefreshScreen(TargetFPS: LongInt); overload;

  //----------------------------------------------------------------------------
  // Colour
  //----------------------------------------------------------------------------

  /// Maps a color from a given bitmap. This is used when determining color
  /// keys for transparent images.
  ///
  /// @param bmp:   the bitmap to get the color for
  /// @param apiColor:     The api color to match
  /// @returns:           The color matched to the bitmaps pixel format
  ///
  /// @lib GetColorForBitmap
  /// @uname GetColorForBitmap
  function GetColor(bmp: Bitmap; apiColor: Color): Color; overload;

  
  /// Gets a color given its RGBA components.
  ///
  /// @param red, green, blue, alpha:  Components of the color
  /// @returns: The matching colour
  ///
  /// @lib GetColor
  function GetColor(red, green, blue, alpha: Byte): Color; overload;

  /// Gets a color given its RGB components.
  ///
  /// @param red, green, blue:   Components of the color
  /// @returns:                 The matching colour
  ///
  /// @lib GetColor(red, green, blue, 255)
  /// @uname GetColorRGB
  function GetColor(red, green, blue: Byte): Color; overload;

  /// @lib
  procedure GetComponents(color: Color; out r, g, b, a: byte);


  /// Returns a color from a floating point RBG value set.
  ///
  /// @param r,g,b: Components for color 0 = none 1 = full
  ///
  /// @lib
  function GetRGBFloatColor(r,g,b: Single): Color;

  /// Returs a color from the HSB input.
  ///
  /// @param hue, saturation, brightness: Values between 0 and 1
  /// @returns The matching color
  ///
  /// @lib
  function GetHSBColor(hue, saturation, brightness: Single): Color;

  /// @lib
  function GetTransparency(color: Color): byte;

  /// @lib
  function GetRed(color: Color): byte;

  /// @lib
  function GetGreen(color: Color): byte;

  /// @lib
  function GetBlue(color: Color): byte;


  //----------------------------------------------------------------------------
  // Refresh / Sleep / Framerate
  //----------------------------------------------------------------------------

  /// Returns the average framerate for the last 10 frames as an integer.
  ///
  /// @returns     The current average framerate
  ///
  /// @lib
  function GetFramerate(): LongInt;

  /// Gets the number of milliseconds that have passed. This can be used to
  /// determine timing operations, such as updating the game elements.
  ///
  /// @returns     The number of milliseconds passed
  ///
  /// @lib
  function GetTicks(): UInt32;

  /// Puts the process to sleep for a specified number of
  /// milliseconds. This can be used to add delays into your
  /// game.
  ///
  /// @param time - The number of milliseconds to sleep
  ///
  /// Side Effects
  /// - Delay before returning
  ///
  /// @lib
  procedure Sleep(time: UInt32);

  /// @lib
  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);


  //----------------------------------------------------------------------------
  // Timers
  //----------------------------------------------------------------------------

  /// @lib
  function CreateTimer(): Timer;

  /// @lib
  procedure FreeTimer(var toFree: Timer);

  /// @lib
  procedure StartTimer(toStart: Timer);

  /// @lib
  procedure StopTimer(toStop: Timer);

  /// @lib
  procedure PauseTimer(toPause: Timer);

  /// @lib
  procedure UnpauseTimer(toUnpause: Timer);

  /// @lib
  function GetTimerTicks(toGet: Timer): UInt32;

  var
    //Preset colours, do not change these values.
    ColorBlue, ColorGreen, ColorRed, ColorWhite, ColorBlack, ColorYellow,
    ColorPink, ColorTurquoise, ColorGrey, ColorMagenta, ColorTransparent,
    ColorLightGrey: Color;

//----------------------------------------------------------------------------
implementation
//----------------------------------------------------------------------------

  uses SysUtils, Math, Classes, sgTrace, sgShared, sgEventProcessing, sgResourceManager, SDL_Image, SDL_gfx;

  type
    // Details required for the Frames per second calculations.
    FPSData = record
      values: Array [0..59] of UInt32;
      pos, loops: LongInt;
      max, min, avg: Single;
    end;

  var
    // Timing details related to calculating FPS
    _lastUpdateTime: UInt32;
    _screen: PSDL_Surface;
    _fpsData: FPSData;

  //---------------------------------------------------------------------------
  // OS X dylib external link
  //---------------------------------------------------------------------------

  {$ifdef DARWIN}
    {$linklib libobjc.dylib}
    procedure NSApplicationLoad(); cdecl; external 'Cocoa'; {$EXTERNALSYM NSApplicationLoad}
    function objc_getClass(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_getClass}
    function sel_registerName(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM sel_registerName}
    function objc_msgSend(self, cmd: LongInt): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_msgSend}
  {$endif}

  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------

  //Used to initialise the Frame Per Second data structure.
  procedure _InitFPSData();
  var
    i: LongInt;
  begin
    // clear the array of values
    for i := Low(_fpsData.values) to High(_fpsData.values) do
      _fpsData.values[i] := 0;
    // zero the current insert position, and the loop count
    _fpsData.pos := 0;
//    _fpsData.loops := 0;
    // set the moving range and average to sensitble defaults
    _fpsData.max := 1;
    _fpsData.min := 1;
    _fpsData.avg := 1000000;
  end;

  //----------------------------------------------------------------------------
  // Library Version
  //----------------------------------------------------------------------------

  function DLLVersion(): LongInt;
  begin
    result := DLL_VERSION;
  end;

  //----------------------------------------------------------------------------
  // Exception Notification/Message
  //----------------------------------------------------------------------------

  function GetExceptionMessage(): String;
  begin
    result := ErrorMessage;
  end;

  function HasExceptionOccured(): Boolean;
  begin
    result := HasException;
  end;

  //----------------------------------------------------------------------------
  // Set Icon / Window Open / Screen Size / Resize
  //----------------------------------------------------------------------------

  procedure _SetupScreen();
  begin
    if screen = nil then New(screen)
    else if (screen^.surface <> nil) then SDL_FreeSurface(screen^.surface);

    with _screen^.format^ do
    begin
      screen^.surface := SDL_CreateRGBSurface(SDL_HWSURFACE,
                                             ScreenWidth(), ScreenHeight(), 32,
                                             RMask, GMask, BMask, SDL_Swap32($000000FF));

      //Turn off alpha blending for when scr is blit onto _screen
      SDL_SetAlpha(screen^.surface, 0, 255);
      SDL_FillRect(screen^.surface, @screen^.surface^.clip_rect, ColorLightGrey);

      baseSurface := screen^.surface;

      screen^.width := _screen^.w;
      screen^.height := _screen^.h;
    end;
  end;

  /// Sets up the graphical window for the specified width and height.
  /// Sets the caption of the window, and the icon if one is specified.
  procedure _InitSDL(caption: String; screenWidth, screenHeight: LongInt);
  var
    icon: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCore', 'InitSDL');
    {$ENDIF}

    if (screenWidth < 1) or (screenHeight < 1) then
      raise Exception.Create('Screen Width and Height must be greater then 0 when opening a Graphical Window');

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

    _screen := SDL_SetVideoMode(screenWidth, screenHeight, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if _screen = nil then
      raise Exception.Create('Unable to create window drawing surface... ' + SDL_GetError());

    _SetupScreen();
    SDL_WM_SetCaption(PChar(caption), nil);

    {$IFDEF TRACE}
      TraceExit('sgCore', 'InitSDL');
    {$ENDIF}
  end;

  procedure SetIcon(filename: String);
  begin
    iconFile := filename;
  end;


  procedure OpenGraphicsWindow(caption: String; width: LongInt; height: LongInt); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCore', 'OpenGraphicsWindow', caption + ': W' + IntToStr(width) + ': H' + IntToStr(height));
    {$ENDIF}

    if screen <> nil then
      raise Exception.Create('Screen has been created. Cannot create multiple windows.');

    try         
      _InitSDL(caption, width, height);
      _InitFPSData();
      
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
      
      SDL_FillRect(screen^.surface, nil, ColorGrey);
      stringColor(screen^.surface, screenWidth div 2 - 30, screenHeight div 2, PChar('Loading ...'), ToGFXColor(ColorWhite));
      RefreshScreen();
    except on e: Exception do
      raise Exception.Create('Error in OpenGraphicsWindow: ' + e.Message);
    end;
    
    LoadResourceBundle('splash.txt', False);
    
    {$IFDEF TRACE}
      TraceExit('sgCore', 'OpenGraphicsWindow');
    {$ENDIF}
  end;
  

  procedure OpenGraphicsWindow(caption: String); overload;
  begin
    OpenGraphicsWindow(caption, 800,600);
  end;

  procedure ToggleFullScreen();
  var
    oldScr: PSDL_Surface;
  begin
    oldScr := _screen;

    try
      _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_FULLSCREEN);
      //Remember... _screen is a pointer to screen buffer, not a "surface"!
    except on exc: Exception do
    end;
  end;

  procedure ChangeScreenSize(width, height: LongInt);
  var
    oldScr: PSDL_Surface;
  begin
    if (screen = nil) then
      raise Exception.Create('Screen has not been created. Unable to get screen width.');

    if (width < 1) or (height < 1) then
      raise Exception.Create('Screen Width and Height must be greater then 0 when resizing a Graphical Window');

    if (width = ScreenWidth()) and (height = ScreenHeight()) then exit;

    oldScr := _screen;
    _screen := SDL_SetVideoMode(width, height, 32, oldScr^.flags);
    _SetupScreen();
  end;

  function ScreenWidth(): LongInt;
  begin
    if (_screen = nil) then
        raise Exception.Create('Screen has not been created. Unable to get screen width.');
    result := _screen^.w;
  end;

  function ScreenHeight(): LongInt;
  begin
    if (_screen = nil) then
        raise Exception.Create('Screen has not been created. Unable to get screen height.');
    result := _screen^.h;
  end;

  procedure TakeScreenShot(basename: String);
  var
    filename: String;
    i: LongInt;
  begin
    filename := basename + '.bmp';
    i := 1;

    while FileExists(filename) do
    begin
      filename := basename + IntToStr(i) + '.bmp';
      i := i + 1;
    end;

    if SDL_SaveBMP(screen^.surface, PChar(filename)) = -1 then
      raise Exception.Create('Failed to save ' + basename + '.bmp: ' + SDL_GetError());
  end;

  //----------------------------------------------------------------------------
  // Refresh / Sleep / Framerate
  //----------------------------------------------------------------------------

  procedure Sleep(time: UInt32);
  begin
    SDL_Delay(time);
  end;

  function GetTicks(): UInt32;
  begin
    result := SDL_GetTicks();
  end;

  function GetFramerate(): LongInt;
  begin
    if _fpsData.avg = 0 then
      result := 60
    else
      result := Round(1000 / _fpsData.avg);
  end;

  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);
  var
    avg, hi, lo: Single;
  begin
    if _fpsData.avg = 0 then
      avg := 9999
    else
      avg := (1000 / _fpsData.avg);

    lo := (1000 / _fpsData.max);
    hi := (1000 / _fpsData.min);

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


  procedure _UpdateFPSData(delta: UInt32);
    function RunningAverage(var values: Array of UInt32; newValue: UInt32; var pos: LongInt): Single;
    var
      i: LongInt;
      sum: Double;
    begin
      // insert the newValue as the position specified
      values[pos] := newValue;
      // calculate the sum for the average
      sum := 0;
      for i := Low(values) to High(values) do
        sum := sum + values[i];
      result := sum / Length(values);
      //Inc position index, and wrap-around to start if needed
      pos := pos + 1;
      if pos > High(values) then pos := Low(values);
    end;
  begin
{    //Populate the running average for the first 10 calcs
    if _fpsData.loops < 10 then
    begin
      //Don't get the avg, because this may result in a
      //div 0 error if avg is 0 (when calcing 1000/avg)
      _RunningAverage(_fpsData.values, delta, _fpsData.pos);
      _fpsData.loops := _fpsData.loops + 1;

      //On the 10th calcs set the min maxes
      if _fpsData.loops = 10 then
      begin
        //Get the running average
        _fpsData.avg := _RunningAverage(_fpsData.values, delta, _fpsData.pos);;
        _fpsData.max := _fpsData.avg;
        _fpsData.min := _fpsData.avg;
        _fpsData.loops := _fpsData.loops + 1;
      end;
    end
    else //All other calcs get the average as normal
      _fpsData.avg := _RunningAverage(_fpsData.values, delta, _fpsData.pos);
}
    _fpsData.avg := RunningAverage(_fpsData.values, delta, _fpsData.pos);

    if _fpsData.avg = 0.0 then _fpsData.avg := 0.01;

    //Adjust the min/maxes
    if _fpsData.avg > _fpsData.max then _fpsData.max := _fpsData.avg;
    if _fpsData.avg < _fpsData.min then _fpsData.min := _fpsData.avg;
  end;



  //----------------------------------------------------------------------------
  // Game Loop Essentials
  //----------------------------------------------------------------------------

  function WindowCloseRequested(): Boolean;
  begin
    if screen = nil then
      result := false
    else
      result := sdlManager.HasQuit();
  end;

  procedure ProcessEvents();
  var
    x, y: LongInt;
  begin
    SDL_GetRelativeMouseState(x, y);
    sdlManager.ProcessEvents();
  end;

  procedure RefreshScreen(); overload;
  var
    nowTime: UInt32;
  begin
    //Draw then delay
    sdlManager.DrawCollectedText(screen^.surface);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);

    nowTime := GetTicks();
    _UpdateFPSData(nowTime - _lastUpdateTime); // delta
    _lastUpdateTime := nowTime;
  end;

  procedure RefreshScreen(TargetFPS: LongInt); overload;
  var
    nowTime: UInt32;
    delta: UInt32;
  begin
    sdlManager.DrawCollectedText(screen^.surface);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);
    
    nowTime := GetTicks();
    delta := nowTime - _lastUpdateTime;

    while (delta < ((1 / TargetFPS) * 1000)) do
    begin
      Sleep(1);
      nowTime := GetTicks();
      delta := nowTime - _lastUpdateTime;
    end;  

    _UpdateFPSData(delta);
    _lastUpdateTime := nowTime;
  end;

  //----------------------------------------------------------------------------
  // Colour
  //----------------------------------------------------------------------------

  procedure GetComponents(color: Color; out r, g, b, a: byte);
  begin
    if baseSurface = nil then
      raise Exception.Create('Cannot read screen format. Ensure window is open.');
    SDL_GetRGBA(color, baseSurface^.Format, @r, @g, @b, @a);
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

  function GetTransparency(color: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    GetComponents(color, r, g, b, a);
    result := a;
  end;

  function GetColor(bmp: Bitmap; apiColor: Color): Color; overload;
  var
    temp: TSDL_Color;
  begin
    if (bmp = nil)
      or (bmp^.surface = nil)
      or (bmp^.surface^.format = nil) then
    begin
      raise Exception.Create('Unable to get color as bitmap not specified');
    end;

    temp := ToSDLColor(apiColor);
    result := SDL_MapRGB(bmp^.surface^.format, temp.r, temp.g, temp.b);
  end;

  function GetColor(red, green, blue, alpha: Byte): Color; overload;
  begin
    if (baseSurface = nil) or (baseSurface^.format = nil) then
      raise Exception.Create('Unable to GetColor as the window is not open');

    try
      result := SDL_MapRGBA(baseSurface^.format, red, green, blue, alpha);
    except
      raise Exception.Create('Error occured while trying to get a color from RGBA components');
    end;
  end;

  function GetColor(red, green, blue: Byte): Color;
  begin
    result := GetColor(red, green, blue, 255);
  end;

  function GetRGBFloatColor(r,g,b: Single): Color;
  begin
    result := GetColor(Round(r * 255), Round(g * 255), Round(b * 255));
  end;

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
    begin // red domain... green ascends
      domainOffset := hue;
      red   := brightness;
      blue  := brightness * (1.0 - saturation);
      green := blue + (brightness - blue) * domainOffset * 6;
    end
    else if hue < 2.0 / 6 then
    begin // yellow domain; red descends
      domainOffset := hue - 1.0 / 6;
      green := brightness;
      blue  := brightness * (1.0 - saturation);
      red   := green - (brightness - blue) * domainOffset * 6;
    end
    else if hue < 3.0 / 6 then
    begin // green domain; blue ascends
      domainOffset := hue - 2.0 / 6;
      green := brightness;
      red   := brightness * (1.0 - saturation);
      blue  := red + (brightness - red) * domainOffset * 6;
    end
    else if hue < 4.0 / 6 then
    begin // cyan domain; green descends
      domainOffset := hue - 3.0 / 6;
      blue  := brightness;
      red   := brightness * (1.0 - saturation);
      green := blue - (brightness - red) * domainOffset * 6;
    end
    else if hue < 5.0 / 6 then
    begin // blue domain; red ascends
      domainOffset := hue - 4.0 / 6;
      blue  := brightness;
      green := brightness * (1.0 - saturation);
      red   := green + (brightness - green) * domainOffset * 6;
    end
    else
    begin // magenta domain; blue descends
      domainOffset := hue - 5.0 / 6;
      red   := brightness;
      green := brightness * (1.0 - saturation);
      blue  := red - (brightness - green) * domainOffset * 6;
    end;

    result := GetRGBFloatColor(red, green, blue);
  end;
  
  //----------------------------------------------------------------------------
  // Timers
  //----------------------------------------------------------------------------
  
  function CreateTimer(): Timer;
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

  procedure StartTimer(toStart: Timer);
  begin
    if not Assigned(toStart) then raise Exception.Create('No timer supplied');
    with toStart^ do
    begin
      started := true;
      paused := false;
      startTicks := SDL_GetTicks();
    end;
  end;

  procedure StopTimer(toStop: Timer);
  begin
    if not Assigned(toStop) then raise Exception.Create('No timer supplied');
    with toStop^ do
    begin
      started := false;
      paused := false;
    end;
  end;

  procedure PauseTimer(toPause: Timer);
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

  procedure UnpauseTimer(toUnpause: Timer);
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

  function GetTimerTicks(toGet: Timer): UInt32;
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


//----------------------------------------------------------------------------
// Global variables for Mac OS Autorelease Pool
//----------------------------------------------------------------------------

{$ifdef DARWIN}
var
  NSAutoreleasePool: LongInt;
  pool: LongInt;
{$endif}


//----------------------------------------------------------------------------

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
  
  try
    if ParamCount() >= 0 then
      applicationPath := ExtractFileDir(ParamStr(0))
    else
      applicationPath := '';
  except
    applicationPath := '';
  end;

  screen := nil;
  baseSurface := nil;

  {$IFDEF TRACE}
    TraceExit('sgCore', 'initialization');
  {$ENDIF}
end;

//----------------------------------------------------------------------------

finalization
begin
  {$IFDEF TRACE}
    TraceEnter('sgCore', 'finalization');
  {$ENDIF}
  
  ReleaseResourceBundle('splash.txt');
  
  if screen <> nil then
  begin
    if screen^.surface <> nil then
      SDL_FreeSurface(screen^.surface);
    
    Dispose(screen);
    screen := nil;
    //scr and baseSurface are now the same!
    baseSurface := nil;
  end;

  SDL_Quit();

  {$IFDEF TRACE}
    TraceExit('sgCore', 'finalization');
  {$ENDIF}
end;

end.