unit SGSDK_Core;

{$IFDEF UNIX}
	{$linklib gcc}
	{$linklib SDLmain}
{$ENDIF}

interface
	uses	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing;

	const
		{$IFDEF FPC}
			EOL = LineEnding;
		{$ELSE}
			{$IFDEF Win32}
				EOL = #13#10;
			{$ELSE}
				{$IFDEF Unix}
					EOL = #10;
				{$ELSE}
					EOL = #13;
				{$ENDIF}
			{$ENDIF}
		{$ENDIF}

		DEG_TO_RAD = 0.0174532925;
		PI = 3.14159265359;
	
	type
		UInt8	= sdl.UInt8;
		UInt16 = sdl.UInt16;
		UInt32 = sdl.UInt32;
		UInt64 = sdl.UInt64;
		
		/// Record: FPSCalcInfo
		///
		/// This record contains details required for the
		/// Frames per second calculations.
		FPSCalcInfo = record
			valuesArray : Array [0..9] of UInt32;
			arrayIndex, loopCount : Integer;
			high, low, average : Single;
		end;

		/// Type: Colour
		///
		/// Colour is used within the SwinGameAPI to store colour values.
		///	This is the same as the Color type.
		Colour = UInt32; //32 bit unsigned

		/// Type : Color
		///
		///	The color type is used within the SwinGameAPI to store color values.
		///	This is the same as the Colour type.
		Color = Colour;
		
		/// Record: Vector
		///
		/// Use Vectors to represent x, y vectors. Vectors can be used to store
		///	values such as movement data. Vectors are stored using floating point
		///	values in order to allow mathematical operations.
		///
		///	- x: The x value of the vector
		///	- y: The y value of the vector
		///	- w: Required for transformation
		Vector = record
			x, y, w : Single;
		end;
		
		/// Record: BitmapData
		///
		///	NOTE: Do not use BitmapData directly, use Bitmap.
		///
		/// - surface: The bitmap's surface, containing the data to be drawn
		/// - width, height: The size of the surface
		/// - nonTransparentPixels: Array used to determine pixel based collisions
		BitmapData = record
			surface: PSDL_Surface;
			width, height: Integer;
			nonTransparentPixels: Array of Array of Boolean;
		end;

		/// Type: Bitmap
		///
		///	The bitmap type is a pointer to a BitmapData. The BitmapData record
		///	contains the data used by the SwinGame API to represent
		///	bitmaps. You can create new bitmaps in memory for drawing operatings
		///	using the CreateBitmap function. This can then be optimised for drawing
		///	to the screen using the OptimiseBitmap routine. Also see the DrawBitmap
		///	routines.
		
		Bitmap = ^BitmapData;
		
	    /// Record: ResourceKind
	    ///
	    ///  Use this with the resource path functions to get the path to a
	    ///  given resource. Using these functions ensures that your resource
	    ///  paths are correct across platforms
	    ResourceKind = (
				FontResource,
				ImageResource,
				SoundResource
		);
		
	var
		//Preset colours, do not change these values.
		ColourBlue, ColorBlue, ColourGreen, ColorGreen,
		ColourRed, ColorRed, ColourWhite, ColorWhite,
		ColourBlack, ColorBlack, ColourYellow, ColorYellow,
		ColourPink, ColorPink, ColourTurquoise, ColorTurquoise,
		ColourGrey, ColorGrey, ColourMagenta, ColorMagenta: Color;
		ColorTransparent, ColourTransparent: Color;
		
		scr: Bitmap;
		applicationPath: String;   //global variable for optimisation...
		sdlManager: TSDLManager;
		
			
		/// The base surface is used to get pixel format information for the
		///	surfaces created. This is used to create colors etc.
		baseSurface: PSDL_Surface;
		
		//Timing details related to calculating FPS
		lastDrawUpdateTime: UInt32;
		renderFPSInfo: FPSCalcInfo;
		
		iconFile: String;
		
		
	procedure ProcessEvents();
	function WindowCloseRequested(): Boolean;
	
	//*****
 		//
	// Graphical window routines.
	//
	//*****
	//
	// These routines are used to setup and work with the graphical window.
	//
	
	procedure SetIcon(iconFilename: String);
	procedure OpenGraphicsWindow(caption : String;
								 width : Integer; height : Integer); overload;
	procedure OpenGraphicsWindow(caption : String); overload;
	
	procedure ChangeScreenSize(width, height: Integer);
	procedure ToggleFullScreen();
	
	procedure RefreshScreen();
	
	procedure TakeScreenshot(basename: String);
	
	function  ScreenWidth(): Integer;
	function  ScreenHeight(): Integer;
	
	//*****
	//
	// General routines
	//
	//*****
	//
	// These routines are used for general purposes.
	//
	
	function	ToSDLColor(color: UInt32): TSDL_Color;
	
	function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload;
	function	GetColour(red, green, blue, alpha: Byte) : Colour; overload;	
	function	GetColour(red, green, blue : Byte) : Colour; overload;
	
	function 	GetRGBFloatColor(r,g,b: Single): Color;
	function 	GetHSBColor(hue, saturation, brightness: Single): Color;

	
	function	GetFramerate(): Integer;
	
	function	GetTicks(): UInt32;
	
	procedure Sleep(time : UInt32);
	
	function GetPathToResource(filename: String; kind: ResourceKind)
		: String; overload;
	
	function GetPathToResource(filename: String): String; overload;
	
	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
	
	function Cos(angle: Single): Single;
	function Sin(angle: Single): Single;
	function Tan(angle: Single): Single;
		
implementation
	uses SysUtils, Math, Classes;
	
	/// ProcessEvents allows the SwinGame API to react to user interactions. This
	///	routine checks the current keyboard and mouse states. This routine must
	///	be called frequently within your game loop to enable user interaction.
	///
	///	Side Effects
	///	- Reads user interaction events
	///	- Updates keys down, text input, etc.
	procedure ProcessEvents();
	begin
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
	
	
	/// Sets up the graphical window for the specified width and height.
	/// Sets the caption of the window, and the icon if one is specified.
	procedure InitSDL(caption: String; screenWidth, screenHeight: Integer);
	var
		icon: PSDL_Surface;
	begin
		if Length(iconFile) > 0 then
		begin
			icon := IMG_Load(PChar(iconFile));
			SDL_WM_SetIcon(icon, 0);
			SDL_FreeSurface(icon);
		end;

		New(scr);
		scr.surface := SDL_SetVideoMode(screenWidth, screenHeight, 32,
															 SDL_HWSURFACE or SDL_DOUBLEBUF);

		with scr.surface.format^ do
		begin
			baseSurface := SDL_CreateRGBSurface(SDL_SWSURFACE or SDL_SRCALPHA,
											 1, 1, 32,
											 RMask, GMask, BMask, SDL_Swap32($000000FF));
		end;

		if scr.surface = nil then
		begin
			raise Exception.Create('Error creating screen with SDL');
		end;
		SDL_WM_SetCaption(PChar(caption), nil);
	end;
	
	procedure ClearArray(var theArray: Array of UInt32);
	var
		index : Integer;
	begin
		for index := Low(theArray) to High(theArray) do
		begin
			theArray[index] := 0;
		end;
	end;
	
	function ToSDLColor(color: UInt32): TSDL_Color;
	begin
		SDL_GetRGB(color, baseSurface^.format, @result.r, @result.g, @result.b);
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
	
	function GetRunningAverage(var runningArray : Array of UInt32;
														 newNumber : UInt32; var index : Integer): Single;
	var
		loopCount : Integer;
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
	
	procedure DoFPSCalculations(var fpsInfo : FPSCalcInfo; 
	                             nowTime, lastUpdateTime : UInt32);
	begin
		//Populate the running average for the first 10 calcs
		if fpsInfo.loopCount < 10 then
		begin
			//Don't get the avg, because this may result in a 
	     //  div 0 error if avg is 0 (when calcing 1000/avg)
			GetRunningAverage(fpsInfo.valuesArray, nowTime - lastUpdateTime, 
	                       fpsInfo.arrayIndex);
			fpsInfo.loopCount := fpsInfo.loopCount + 1;
		end
		//All other calcs get the average as normal
		else
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
	///	graphics window. The icon is loaded as a bitmap, though this can be from
	///	any kind of bitmap file.
	///
	///	@param iconFilename:	 The name of the file to load as the images icon
	///
	///Side Effects
	///	- The icon will be loaded and used as the windows icon when the window
	///	is opened.
	procedure SetIcon(iconFilename: String);
	begin
		iconFile := iconFilename;
	end;
	
		/// Switches the application to full screen or back from full screen to
	///	windowed.
	///
	/// Side Effects:
	///	- The window switched between fullscreen and windowed
	procedure ToggleFullScreen();
	var
		oldScr: PSDL_Surface;
	begin
		oldScr := scr.surface;
		scr.surface := SDL_SetVideoMode(oldScr.w, oldScr.h, 32, 
	                                   oldScr.flags xor SDL_FULLSCREEN);
		SDL_FreeSurface(oldScr);
	end;
	
	/// Changes the size of the screen.
	///
	/// @param width, height:	The new width and height of the screen
	///
	/// Side Effects:
	/// - The screen changes to the specified size
	procedure ChangeScreenSize(width, height: Integer);
	var
		oldScr: PSDL_Surface;
	begin
		oldScr := scr.surface;
		scr.surface := SDL_SetVideoMode(width, height, 32, oldScr.flags);
		SDL_FreeSurface(oldScr);
	end;
	
	/// Returns the width of the screen currently displayed.
	///
	/// @returns:	The screen's width
	function ScreenWidth(): Integer;
	begin
		result := scr.surface.w;
	end;
	
	/// Returns the height of the screen currently displayed.
	///
	/// @returns:	The screen's height
	function ScreenHeight(): Integer;
	begin
		result := scr.surface.h;
	end;	
	
	/// Opens the graphical window so that it can be drawn onto. You can set the
	///	icon for this window using SetIcon. The window itself is only drawn when
	///	you call RefreshScreen. All windows are opened at 32 bits per pixel. You
	///	can toggle fullscreen using ToggleFullScreen. The window is closed when
	///	the application terminates.
	///
	///	@params caption: The caption for the window
	///	@params width:	 The width of the window
	///	@params height:	The height of the window
	///
	/// Side Effects:
	///	- A graphical window is opened
	procedure OpenGraphicsWindow(caption : String; 
	                              width : Integer; height : Integer); overload;
	begin
		InitSDL(caption, width, height);
		InitFPSCalcInfo(renderFPSInfo);
	
		//Init the colors
		ColorWhite := GetColour(255, 255, 255, 255);
		ColorGreen := GetColour(0, 255, 0, 255);
		ColorBlue := GetColour(0, 0, 255, 255);
		ColorBlack := GetColour(0, 0, 0, 255);
		ColorRed := GetColour(255, 0, 0, 255);
		ColorYellow := GetColour(255, 255, 0, 255);
		ColorPink := GetColour(255, 20, 147, 255);
		ColorTurquoise := GetColour(0, 206, 209, 255);
		ColorGrey := GetColour(128, 128, 128, 255);
		ColorMagenta := GetColour(255, 0, 255, 255);
		ColorTransparent := GetColour(0, 0, 0, 0);
	
		ColourWhite := ColorWhite;
		ColourGreen := ColorGreen;
		ColourBlue := ColorBlue;
		ColourBlack := ColorBlack;
		ColourRed := ColorRed;
		ColourYellow := ColorYellow;
		ColourPink := ColorPink;
		ColourTurquoise := ColorTurquoise;
		ColourGrey := ColorGrey;
		ColourMagenta := ColorMagenta;
		ColourTransparent := ColorTransparent;
	end;
	
	/// Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
	///	for more options.
	///	@params caption: The caption for the window
	///
	/// Side Effects:
	///	- A graphical window is opened
	
	procedure OpenGraphicsWindow(caption : String); overload;
	begin
		OpenGraphicsWindow(caption, 800,600);
	end;
	
	
	/// Draws the current drawing to the screen. This must be called to display
	///	anything to the screen. This will draw all drawing operations, as well
	///	as the text being entered by the user.
	///
	/// Side Effects:
	///	- The current drawing is shown on the screen.
	procedure RefreshScreen();
	var
		nowTime: UInt32;
	begin
		//Update the last time drawn...
		nowTime := GetTicks();
		DoFPSCalculations(renderFPSInfo, nowTime, lastDrawUpdateTime);
		lastDrawUpdateTime := nowTime;
	
		sdlManager.DrawCollectedText(scr.surface);
		SDL_Flip(scr.surface);
		//SDL_UpdateRect(scr,0,0,0,0);
	end;
	
	/// Saves the current screen a bitmap file. The file will be saved into the
	///	current directory.
	///
	///	@params basename	 The base name for the screen shot. e.g. "GemCollector"
	///
	/// Side Effects:
	///	- Saves the current screen image to a bitmap file.
	procedure TakeScreenShot(basename: String);
	var
		filename: String;
		i : Integer;
	begin
		filename := '' + basename + '.bmp';
		i := 1;
		while FileExists(filename) do
		begin
			filename := '"./' + basename + IntToStr(i) + '.bmp"';
			i := i + 1;
		end;
		SDL_SaveBMP(scr.surface, PChar(filename));
	end;
	
	/// Maps a color from a given bitmap. This is used when determining color
	///	keys for transparent images.
	///
	///	@param forBitmap:		the bitmap to get the color for
	///	@param apiColor:		 The api color to match
	///	@returns:						The color matched to the bitmaps pixel format
	function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload;
	var
		temp: TSDL_Color;
	begin
		temp := ToSDLColor(apiColor);
		result := SDL_MapRGB(forBitmap.surface.format, temp.r, temp.g, temp.b);
	end;
	
	/// Gets a color given its RGBA components.
	///
	///	@param red, green, blue, alpha:	 Components of the color
	///	@returns: The matching colour
	function GetColour(red, green, blue, alpha: Byte) : Colour; overload;
	begin
		result := SDL_MapRGBA(baseSurface.format, red, green, blue, alpha);
	end;
	
	/// Gets a color given its RGB components.
	///
	///	@param red, green, blue:	 Components of the color
	///	@returns:									The matching colour
	function GetColour(red, green, blue : Byte) : Colour;
	begin
		result := GetColour(red, green, blue, 255);
	end;
	
	/// Returns a color from a floating point RBG value set.
	///
	/// @param r,g,b:	Components for color 0 = none 1 = full
	function GetRGBFloatColor(r,g,b: Single): Color;
	begin
		result := GetColour(Round(r * 255), Round(g * 255), Round(b * 255));
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
			red 	:= brightness;
			blue  	:= brightness * (1.0 - saturation);
        	green 	:= blue + (brightness - blue) * domainOffset * 6;
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
	///	- Delay before returning
	procedure Sleep(
		time : UInt32);
	begin
		SDL_Delay(time);
	end;
	
	/// Checks to see if the window has been asked to close. You need to handle
	///	this if you want the game to end when the window is closed. This value
	///	is updated by the ProcessEvents routine.
	///
	/// @returns : True if the window has been requested to close.
	function WindowCloseRequested(): Boolean;
	begin
		result := sdlManager.HasQuit(); //WndCloseRequested();
	end;
	
	/// Gets the number of milliseconds that have passed. This can be used to
	///	determine timing operations, such as updating the game elements.
	///
	///	@returns		 The number of milliseconds passed
	function GetTicks(): UInt32;
	begin
		result := SDL_GetTicks();
	end;
	
	/// Returns the average framerate for the last 10 frames as an integer.
	///
	///	@returns		 The current average framerate
	function GetFramerate(): Integer;
	begin
		if renderFPSInfo.average = 0 then
			result := 9999
		else
			result := Round(1000 / renderFPSInfo.average);
	end;
	
	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
	begin
		sdlManager.RegisterEventProcessor(handle, handle2);
	end;
	
	function GetPathToResource(filename: String): String; overload;
	begin
	{$ifdef UNIX}
		{$ifdef DARWIN}
			result := applicationPath + '/../Resources/';
		{$else}
			result := applicationPath + '/Resources/';
		{$endif}
	{$else}
	//Windows
		result := applicationPath + '\resources\';
	{$endif}

	result := result + filename;
	end;

	function GetPathToResource(filename: String; kind: ResourceKind) : String; overload;
	begin
		case kind of
			{$ifdef UNIX}
				FontResource: result := GetPathToResource('fonts/' + filename);
				SoundResource: result := GetPathToResource('sounds/' + filename);
				ImageResource: result := GetPathToResource('images/' + filename);
			{$else}
				FontResource: result := GetPathToResource('fonts\' + filename);
				SoundResource: result := GetPathToResource('sounds\' + filename);
				ImageResource: result := GetPathToResource('images\' + filename);
			{$endif}
	end;
end;
		
initialization
begin
	WriteLn('InitSDL');
	if SDL_Init(SDL_INIT_EVERYTHING) = -1 then
	begin
		WriteLn('Errorm loading sdl...');
		WriteLn(string(SDL_GetError));
		raise Exception.Create('Error initialising SDL. ' + string(SDL_GetError));
	end;
	
	SDL_EnableUNICODE(SDL_ENABLE);
	
	sdlManager := TSDLManager.Create();
	applicationPath := ExtractFileDir(ParamStr(0));
	
	//Load sound
	{WriteLn('Opening Mixer');
	if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 1024 ) = -1 then
	begin
		WriteLn('Errorm loading mixer...');
		WriteLn(string(Mix_GetError));
		//raise Exception.Create('Error openning audio device. ' + string(Mix_GetError));
	end;
	
	WriteLn('Mixer Open');}
end;

finalization
begin
	WriteLn('Closing Down');
	if sdlManager <> nil then
	begin
		sdlManager.Free();
		sdlManager := nil;
	end;

	if scr <> nil then
	begin
		if scr.surface <> nil then
		begin
			SDL_FreeSurface(scr.surface);
		end;
		scr.surface := nil;

		Dispose(scr);
		scr := nil;
	end;
	
	WriteLn('Closing TTF');
	//TTF_Quit();
	WriteLn('Closed TTF');
	
	WriteLn('Quitting SDL');
	SDL_Quit();
	WriteLn('Quit');
end;
end.