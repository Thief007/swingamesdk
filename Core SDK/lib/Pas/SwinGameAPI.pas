unit SwinGameAPI;

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
	
	type
		UInt8	= sdl.UInt8;
		UInt16 = sdl.UInt16;
		UInt32 = sdl.UInt32;
		UInt64 = sdl.UInt64;

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

		/// Type: Font
		///
		/// Fonts are used to render text to bitmaps and to the screen.
		/// Fonts must be loaded using the CreateFont routine. Also see the
		///	DrawText and DrawTextLines routines.
		Font = PTTF_Font;

		 /// Enumeration: FontStyles
		///
		/// Use font styles to set the style of a font. Setting the style is time
		///	consuming, so create alternative font variables for each different
		///	style you want to work with. Note that these values can be logical
		///	ORed together to combine styles, e.g. BoldFont or ItalicFont = both
		///	bold and italic.
		FontStyle = (
				NormalFont		= TTF_STYLE_NORMAL,
				BoldFont			 = TTF_STYLE_BOLD,
				ItalicFont		 = TTF_STYLE_ITALIC,
				UnderlineFont = TTF_STYLE_UNDERLINE
			);

		/// Enumeration: FontAlignment
		///
		///	Use font alignment for certain drawing operations. With these
		///	operations you specify the area to draw in as well as the alignment
		///	within that area. See DrawTextLines.
		FontAlignment = (
				AlignLeft =	1,
				AlignCenter =	2,
				AlignRight =	4
			);


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

    /// type: Matrix2d
    ///
    ///  This record is used to represent transformations that can be
    ///  used to apply these changes to vectors.
		Matrix2D = Array [0..2,0..2] of Single;

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

		/// Record: Sprite
		///
		///	NOTE: Do not use SpriteData directly. Use Sprite.
		///
		///	- bitmaps: The array of bitmaps related to the Sprite
		///	- xPod, yPos: The sprites location within the game world
		///	- currentFrame: The current animation frame for the Sprite
		///	- usePixelCollision: A flag indicating if pixel collision sould be
		///											 used, if false bounding collision is used.
		SpriteData = record
			bitmaps : Array of Bitmap;
			xPos : Single;
			yPos : Single;
			currentFrame : Integer;
			usePixelCollision: Boolean;
		end;

		/// Type: Sprite
		///
		///	Sprites are used to represent Sprites drawn to the screen. Create a
		///	sprite using the CreateSprite function, and free it when complete with
		///	the FreeSprite function. The sprite contain a number of bitmaps used to
		///	store animations, or the like. Sprite drawing operations will draw the
		///	Sprite's current frame.
		Sprite = ^SpriteData;

		/// Record: DecayProperties
		///
		///	These properties contain a number of values that can be used to decay
		///	vectors applied to sprites in a game. This allows for things such as
		///	gravety, and provides a means for slowing a character to stop rather
		///	than stopping immediately when a key is released.
		///
		///	- posXDecay: The amount to decay the X component towards zero if the
		///							 X component is positive
		///	- negXDecay: The amount to decay the X component towards zero if the
		///							 X component is negative
		///	- posYDecay: The amount to decay the Y component towards zero if the
		///							 Y component is positive
		///	- negYDecay: The amount to decay the Y component towards zero if the
		///							 Y component is negative
		DecayProperties = record
			posXDecay : Single;
			negXDecay : Single;
			posYDecay : Single;
			negYDecay : Single;
		end;

		/// Type: SoundEffect
		///
		/// Use sound effects to play sounds in a game. Load sound effects using th
		///	LoadSoundEffect routine. You can then PlaySoundEffect to start the
		///	effect playing.
		///
		/// NOTE: Do not use this to play music, see the Music type.
		SoundEffect = PMix_Chunk;

		/// Type: Music
		///
		/// Background music is played on a loop. Use this music type
		/// for variables that refer to music that can be played. You can load
		///	these using LoadMusic, play with PlayMusic, and stop with StopMusic.
		///	Also see the IsMusicPlaying routine.
		Music = PMix_Music;

		/// Enumeration: CollisionDetectionRanges
		///	This is used to indicate the kind of collision being checked with the
		///	Sprite collision routines. 
		CollisionDetectionRange = (
				CollisionRangeEquals			= 0,
				CollisionRangeGreaterThan = 1,
				CollisionRangeLessThan		= 2
			);

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

  //
	//
	// Input handling routines.
	//
	//*****
	//
	// These routines are used to handle input from the user.
	//


	procedure ProcessEvents();
	procedure StartReadingText(textColor: Colour; maxLength: Integer;
														 theFont: Font; x, y: Integer);

	function WindowCloseRequested(): Boolean;

	function IsReadingText(): Boolean;
	function TextReadAsASCII(): String;
	function TextReadAsUNICODE(): WideString;
	function IsKeyPressed(virtKeyCode : Integer): Boolean;
	function WasKeyTyped(virtKeyCode: Integer): Boolean;

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
	// Resource loading and freeing routines
	//
	//*****
	//
	// These routines are used to load resources, and to free them.
	//

	function CreateBitmap(width, height: Integer): Bitmap;

	procedure OptimiseBitmap(surface: Bitmap);

	function LoadBitmap(pathToBitmap: String; transparent: Boolean;
								transparentColor: Colour): Bitmap; overload;
	function LoadBitmap(pathToBitmap : String): Bitmap; overload;

	function LoadTransparentBitmap(pathToBitmap : String;
								transparentColor : Colour): Bitmap; overload;

	procedure FreeBitmap(var bitmapToFree : Bitmap);

	function LoadFont(fontName: String; size: Integer): Font;

	procedure SetFontStyle(font: Font; style: FontStyle);

	procedure FreeFont(var fontToFree: Font);

	//*****
	//
	// Bitmap drawing routines
	//
	//*****
	//
	// These routines are used to draw to a bitmap.
	//

	procedure ClearSurface(dest: Bitmap; toColour: Colour); overload;
	procedure ClearSurface(dest: Bitmap); overload;

	procedure DrawBitmap(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
		overload;

	procedure DrawBitmapPart(dest: Bitmap; bitmapToDraw: Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); overload;

	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour;
					theFont: Font; x, y: Integer); overload;

	procedure DrawTextLines(dest: Bitmap; theText: String;
							textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); overload;

	procedure DrawPixel(dest: Bitmap; theColour: Colour; x, y: Integer);
		overload;

	procedure DrawRectangle(dest: Bitmap; theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); overload;

	procedure DrawRectangle(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure FillRectangle(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure DrawLine(dest: Bitmap; theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); overload;

	procedure DrawHorizontalLine(dest: Bitmap; theColor: Color;
								 y, x1, x2: Integer); overload;

	procedure DrawVerticalLine(dest: Bitmap; theColor: Color;
							 x, y1, y2: Integer); overload;

	procedure DrawCircle(dest: Bitmap; theColour: Colour; filled: Boolean;
							 xc, yc, radius: Integer); overload;

	procedure DrawCircle(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); overload;

	procedure FillCircle(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); overload;

	procedure DrawEllipse(dest: Bitmap; theColour: Colour; filled: Boolean;
							xPos, yPos, width, height: Integer); overload;

	procedure DrawEllipse(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); overload;
	
	procedure FillEllipse(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); overload;

	//*****
	//
	// Screen drawing routines
	//
	//*****
	//
	// These routines are used to draw directly to the screen.
	//

	procedure ClearScreen(toColour : Colour); overload;

	procedure ClearScreen(); overload;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Integer); overload;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); overload;

	procedure DrawText(theText: String; textColor: Colour;
					 theFont: Font; x, y: Integer); overload;

	procedure DrawTextLines(theText: String; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); overload;

	procedure DrawPixel(theColour: Colour; x, y: Integer); overload;

	procedure DrawRectangle(theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); overload;

	procedure DrawRectangle(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure FillRectangle(theColour : Colour; xPos, yPos,
							width, height : Integer); overload;

	procedure DrawLine(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); overload;

	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Integer); overload;

	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Integer); overload;

	procedure DrawCircle(theColour: Colour; filled: Boolean;
						 xc, yc, radius: Integer); overload;

	procedure DrawCircle(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure FillCircle(theColour: Colour; xc, yc, radius: Integer); overload;

	procedure DrawEllipse(theColour: Colour; filled: Boolean;
						xPos, yPos, width, height: Integer); overload;

	procedure DrawEllipse(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;

	 procedure FillEllipse(theColour: Colour;
						xPos, yPos, width, height: Integer); overload;

	//*****
	//
	// Sprite routines
	//
	//*****
	//
	// These routines are used to work with Sprites within your game.
	//

	function	CreateSprite(startBitmap : Bitmap): Sprite;

	procedure FreeSprite(var spriteToFree : Sprite);

	function AddBitmapToSprite(spriteToAddTo : Sprite;
														 bitmapToAdd : Bitmap): Integer;

	function CurrentHeight(sprite: Sprite): Integer;

	function CurrentWidth(sprite: Sprite): Integer;

	procedure DrawSprite(spriteToDraw : Sprite); overload;

	procedure DrawSprite(spriteToDraw : Sprite; vwPrtX, vwPrtY, vwPrtWidth,
											 vwPrtHeight : Integer); overload;

	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector);

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer);

  function IsSpriteOffscreen(theSprite : Sprite): Boolean; overload;

	function	IsSpriteOffscreen(theSprite : Sprite; vwPrtX, vwPrtY,
															vwPrtWidth, vwPrtHeight : Integer)
                              : Boolean; overload;

	//*****
	//
	// Collision detection routines
	//
	//*****
	//
	// These routines are used to detect collisions between sprites or bitmaps.
	//

	function	HasSpriteCollidedX(theSprite : Sprite; x : Integer;
															 range : CollisionDetectionRange): Boolean;

	function HasSpriteCollidedY(theSprite : Sprite; y : Integer;
															 range : CollisionDetectionRange): Boolean;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Boolean; overload;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height: Integer; vwPrtX, vwPrtY: Integer)
		: Boolean; overload;
		
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;

	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
		x, y: Integer; bounded: Boolean;
		vwPrtX, vwPrtY: Integer)
		: Boolean; overload;

	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y: Integer; bounded: Boolean)
																				: Boolean; overload;

	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y, vwPrtX, vwPrtY: Integer)
																				: Boolean; overload;

	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y: Integer)
																				: Boolean; overload;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2 : Bitmap;
															 x2, y2: Integer): Boolean; overload;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Boolean; overload;

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
		: Boolean; overload;

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Boolean; overload;
	//*****
	//
	// Vector routines
	//
	//*****
	//
	// These routines are used to manipulate vectors in the API.
	//

	function	CreateVector(x,y : Single; invertY : boolean): Vector; overload;

	function	CreateVector(x,y : Single): Vector; overload;

	function	AddVectors(v1, v2 : Vector): Vector;

	function	SubtractVectors(v1, v2 : Vector): Vector;

	function	InvertVector(v : Vector): Vector;

	function	ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer): Vector;

	function	LimitVector(theVector: Vector; maxMagnitude: Single): Vector;

	function	GetUnitVector(theVector : Vector): Vector;

	function	IsZeroVector(theVector : Vector): Boolean;

	function	GetVectorMagnitude(theVector : Vector): Single;

	function	DecayVector(theVector : Vector;
												const decayProps : DecayProperties): Vector;
	function	DotProduct(v1, v2: Vector): Single;
	function 	MultiplyVector(v1: Vector; s1: Single): Vector;

	//*****
	//
	// Sound routines
	//
	//*****
	//
	// These routines are used to work with sound effects and music within the
	// game API.
	//

	function	LoadSoundEffect(path: String): SoundEffect;

	function	LoadMusic(path: String): Music;

	procedure FreeMusic(var mus: Music);

	procedure FreeSoundEffect(var effect: SoundEffect);

	procedure PlaySoundEffect(effect: SoundEffect); overload;

	procedure PlaySoundEffect(effect: SoundEffect; loops: Integer); overload;

	procedure PlayMusic(mus: Music; loops: Integer); overload;

	procedure PlayMusic(mus: Music); overload;

	function	IsMusicPlaying(mus: Music): Boolean;

	function	IsSoundEffectPlaying(effect: SoundEffect): Boolean;

	procedure StopSoundEffect(effect: SoundEffect);

	procedure StopMusic();

	//*****
	//
	// General routines
	//
	//*****
	//
	// These routines are used for general purposes.
	//

	function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload;

	function	GetColour(red, green, blue, alpha: Byte) : Colour; overload;

	function	GetColour(red, green, blue : Byte) : Colour; overload;

	function	TextWidth(theText: String; theFont: Font): Integer;

	function	TextHeight(theText: String; theFont: Font): Integer;

	procedure DrawFramerate(x, y: Integer; font: Font);

	function	GetFramerate(): Integer;

	function	GetTicks(): UInt32;

	procedure Sleep(time : UInt32);

 	function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
	function CalculateAngle(sprite1, sprite2: Sprite): Single; overload;

	function TranslationMatric(dx, dy: Single): Matrix2D;
	function ScaleMatrix(scale: Single): Matrix2D;
	function RotationMatrix(deg: Single): Matrix2D;
	function Multiply(const m1, m2: Matrix2D): Matrix2D; overload;
	function Multiply(const m: Matrix2D; const v: Vector): Vector; overload;
		
	function GetPathToResource(filename: String; kind: ResourceKind)
		: String; overload;
		
	function GetPathToResource(filename: String): String; overload;
	
	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);

	function Cos(angle: Single): Single;
	function Sin(angle: Single): Single;
	function Tan(angle: Single): Single;

implementation
	uses SysUtils, Math,
			 Classes, SwinGameDrawing;

	/// Record: FPSCalcInfo
	///
	/// This record contains details required for the
	/// Frames per second calculations.
	type FPSCalcInfo = record
			valuesArray : Array [0..9] of UInt32;
			arrayIndex, loopCount : Integer;
			high, low, average : Single;
		end;

	var
		/// Object to process SDL events in the background
		sdlManager: TSDLManager;

		// Contains the sound channels used to determine if a sound is currently
		// playing and enables us to stop the sound, check if it is playing etc.
		soundChannels: Array[0..7] of Pointer;
		scr: Bitmap;

		/// The base surface is used to get pixel format information for the
		///	surfaces created. This is used to create colors etc.
		baseSurface: PSDL_Surface;

		//Timing details related to calculating FPS
		lastDrawUpdateTime: UInt32;
		renderFPSInfo: FPSCalcInfo;

		iconFile: String;

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

	procedure ClearArray(var theArray: Array of UInt32);
	var
		index : Integer;
	begin
		for index := Low(theArray) to High(theArray) do
		begin
			theArray[index] := 0;
		end;
	end;

	//*****
	//
	// General Routines....
	//
	//*****
	//
	// These routines are private to this unit, and exist to provide utility
	// operations for the SwinGameAPI.

	function NewSDLRect(x, y, w, h: Integer): SDL_Rect;
	begin
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

	function ToSDLColor(color: UInt32): TSDL_Color;
	begin
		{$IF SDL_BYTEORDER = SDL_BIG_ENDIAN } //4321 = ARGB
			result.r := (color and $00FF0000) shr 16;
			result.g := (color and $0000FF00) shr 8;
			result.b := (color and $000000FF);
		{$ELSE} //1234 = BGRA
			result.r := (color and $0000FF00) shr 8;
			result.g := (color and $00FF0000) shr 16;
			result.b := (color and $FF000000) shr 24;
		{$IFEND}
	end;

	function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
	begin
		result := (Integer(toCheck) and Integer(checkFor)) = Integer(checkFor);
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

	/// Clears the surface of the bitmap to the passed in color.
	///
	///	@param dest:		 The bitmap to clear
	///	@param toColour: The colour to clear the bitmap to
	///
	/// Side Effects:
	///	- dest's surface is set to the toColor
	procedure ClearSurface(dest: Bitmap; toColour: Colour); overload;
	begin
		SDL_FillRect(dest.surface, @dest.surface.clip_rect, toColour);
	end;

 	/// Clears the surface of the bitmap to Black.
	///
	///	@param dest:		 The bitmap to clear
	///
	/// Side Effects:
	///	- dest's surface is set to black
	procedure ClearSurface(dest: Bitmap); overload;
	begin
		ClearSurface(dest, ColorBlack);
	end;

	/// Clears the surface of the screen to the passed in color.
	///
	///	@param toColour: The colour to clear the bitmap to
	///
	/// Side Effects:
	///	- Screen's surface is set to the toColor
	procedure ClearScreen(toColour : Colour); overload;
	begin
		ClearSurface(scr, toColour);
	end;

	/// Clears the screen to Black.
	///
	/// Side Effects:
	///	- screen's surface is set to black
	procedure ClearScreen(); overload;
	begin
		ClearScreen(ColorBlack);
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
	///	@returns:												 The matching colour
	function GetColour(red, green, blue, alpha: Byte) : Colour; overload;
	begin
		//result := red shl baseSurface.format.Rshift;
		//result := result or (green shl baseSurface.format.Gshift);
		//result := result or (blue shl baseSurface.format.Bshift);
		//result := result or (alpha shl baseSurface.format.Ashift);

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

	function GetPixel32(surface: PSDL_Surface; x, y: Integer): Colour;
	var
		pixels, pixel: PUint32;
		offset, pixelAddress: Integer;
	begin
		//Convert the pixels to 32 bit
		pixels := surface.pixels;

		//Get the requested pixel
		offset := (( y * surface.w ) + x) * surface.format.BytesPerPixel;
		pixelAddress := Integer(pixels) + offset;

		{$IFDEF FPC}
			pixel := PUint32(pixelAddress);
		{$ELSE}
			pixel := Ptr(pixelAddress);
		{$ENDIF}

		{$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
		case surface.format.BytesPerPixel of
			1: result := pixel^ and $000000ff;
			2: result := pixel^ and $0000ffff;
			3: result := pixel^ and $00ffffff;
			4: result := pixel^;
		else
			raise Exception.Create('Unsuported bit format...');
		end;
		{$ELSE}
		case surface.format.BytesPerPixel of
			1: result := pixel^ and $ff000000;
			2: result := pixel^ and $ffff0000;
			3: result := pixel^ and $ffffff00;
			4: result := pixel^;
		else
			raise Exception.Create('Unsuporte bit format...');
		end;
		{$IFEND}
	end;

	// Sets the non-transparent pixels in a Bitmap. This is then used for
	// collision detection, allowing the original surface to be optimised.
	//
	// @param toSet	 A pointer to the Bitmap being set
	// @param surface The surface with pixel data for this Bitmap
	procedure SetNonTransparentPixels(toSet: Bitmap; surface: PSDL_Surface;
																		transparentColor: Color);
	var
		r, c: Integer;
	begin
		SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);

		for c := 0 to toSet.width - 1 do
		begin
			for r := 0 to toSet.height - 1 do
			begin
				toSet.nonTransparentPixels[c, r] :=
					(GetPixel32(surface, c, r) <> transparentColor);
			end;
		end;
	end;

	procedure SetNonAlphaPixels(toSet: Bitmap; surface: PSDL_Surface);
	var
		r, c: Integer;
		hasAlpha: Boolean;
	begin
		SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);
		hasAlpha := surface.format.BytesPerPixel = 4;

		for c := 0 to toSet.width - 1 do
		begin
			for r := 0 to toSet.height - 1 do
			begin
				toSet.nonTransparentPixels[c, r] := hasAlpha and
					((GetPixel32(surface, c, r) and SDL_Swap32($000000FF)) > 0);
			end;
		end;
	end;

	/// Loads a bitmap from a given path, with the indicated transparent color.
	/// This loads both transparent and non-transparent bitmaps.
	///
	///	@param pathToBitmap:		 the path to the bitmap to be loaded
  /// @param transparent:      Indicates if transparency should be set
	///	@param transparentColor: the color that will be transparent
	///	@returns: A bitmap from the loaded file.
	function	LoadBitmap(pathToBitmap: String; transparent: Boolean;
											 transparentColor: Colour): Bitmap; overload;
	var
		loadedImage: PSDL_Surface;
		correctedTransColor: Colour;
	begin
		loadedImage := IMG_Load(pchar(pathToBitmap));

		if loadedImage <> nil then
		begin
			new(result);
			if not transparent then
				result.surface := SDL_DisplayFormatAlpha(loadedImage)
			else
				result.surface := SDL_DisplayFormat(loadedImage);

			result.width := result.surface.w;
			result.height := result.surface.h;

			if transparent then
			begin
				correctedTransColor := GetColour(result, transparentColor);
				SDL_SetColorKey(result.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, 
                        correctedTransColor);
				SetNonTransparentPixels(result, loadedImage, correctedTransColor);
			end
			else
			begin
				SetNonAlphaPixels(result, loadedImage);
			end;

			SDL_FreeSurface(loadedImage);
		end
		else
		begin
			raise Exception.Create('Error loading image: ' + 
                             pathToBitmap + ': ' + SDL_GetError());
		end;
	end;

	/// Loads a bitmap from file into a Bitmap variable. This can then be drawn to
	///	the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
	///	contain alpha values, which will be drawn correctly by the API. All
	///	bitmaps must be freed using the FreeBitmap once you are finished with
	///	them.
	///
	///	@param pathToBitmap:	 The path to the bitmap file to open.
	///	@returns: A bitmap from the loaded file
	function LoadBitmap(pathToBitmap : String): Bitmap; overload;
	begin
		result := LoadBitmap(pathToBitmap, false, ColorBlack);
	end;

 	/// Loads a bitmap with a transparent color key. The transparent color is then
	///	setup as the color key to ensure the image is drawn correctly. Alpha
	///	values of Images loaded in this way will be ignored. All bitmaps must be
	///	freed using the FreeBitmap once you are finished with them.
	///
	///	@param pathToBitmap:		 the path to the bitmap to be loaded
	///	@param transparentColor: the color that will be transparent
	///	@returns: A bitmap from the loaded file.
	function LoadTransparentBitmap(pathToBitmap : String;
                                 transparentColor : Colour): Bitmap; overload;
	begin
		result := LoadBitmap(pathToBitmap, true, transparentColor);
	end;

	/// Frees a loaded bitmap. Use this when you will no longer be drawing the
	///	bitmap, and when the program exits.
	///
	/// Side Effects:
	///	- the bitmap is freeed and can no longer be drawn
	procedure FreeBitmap(var bitmapToFree : Bitmap);
	begin
		if bitmapToFree <> nil then
		begin
			if bitmapToFree.surface <> nil then
			begin
				SDL_FreeSurface(bitmapToFree.surface);
			end;
			bitmapToFree.surface := nil;

			Dispose(bitmapToFree);
			bitmapToFree := nil;
		end;
	end;

	/// Creates a sprites, and sets its firat bitmap.
	///
	///	@param startBitmap:		The sprites first bitmap (index 0)
	///	@returns:							A new sprite with this bitmap as its first bitmap
	function CreateSprite(startBitmap : Bitmap): Sprite;
	begin
		New(result);
		SetLength(result.bitmaps, 1);

		result.bitmaps[0]						:= startBitmap;
		result.xPos									:= 0;
		result.yPos									:= 0;
		result.currentFrame					:= 0;
		result.usePixelCollision		 := false;
	end;

	/// Frees a sprite, this does not free the sprite's bitmaps, which allows
	///	bitmaps to be shared between sprites. All created sprites need to be
	///	freed.
	///
	///	@param spriteToFree:		 the sprite to free
	///
	/// Side Effects:
	///	- The sprites details are cleaned up.
	procedure FreeSprite(var spriteToFree : Sprite);
	begin
		//for index := 0 to High(spriteToFree.bitmaps) do
		//begin
		//	FreeBitmap(spriteToFree.bitmaps[index]);
		//end;

		SetLength(spriteToFree.bitmaps, 0);
		Dispose(spriteToFree);
		spriteToFree := nil;
	end;

	/// Sprites may contain multiple images. These images can be used for things
	///	line animation, facing, etc. This routine adds a bitmap to a sprite,
	///	returning the index of the added bitmap.
	///
	///	@param spriteToAddTo:		the sprite to add the bitmap to
	///	@param bitmapToAdd:			the bitmap to add to the sprite
	///	@returns :							 the index of the added bitmap
	///
	/// Side Effects:
	///	- The bitmaps is added to the bitmaps within the sprite.
	function AddBitmapToSprite(spriteToAddTo : Sprite;
														 bitmapToAdd : Bitmap): Integer;
	begin
		//Resize the array
		SetLength(spriteToAddTo.bitmaps, Length(spriteToAddTo.bitmaps) + 1);

		//Add the values to the array
		spriteToAddTo.bitmaps[High(spriteToAddTo.bitmaps)] := bitmapToAdd;

		result := High(spriteToAddTo.bitmaps);
	end;

	/// Returns the current width of the sprite.
	///
	///	@param sprite:		 The sprite to get the width of
	///	@returns					 The width of the sprite's current frame
	function CurrentWidth(sprite: Sprite): Integer;
	begin
		result := sprite.bitmaps[sprite.currentFrame].width;
	end;
  
	/// Returns the current height of the sprite.
	///
	///	@param sprite:		 The sprite to get the height of
	///	@returns					 The height of the sprite's current frame
	function CurrentHeight(sprite: Sprite): Integer;
	begin
		result := sprite.bitmaps[sprite.currentFrame].height;
	end;

 	/// Draws one bitmap (bitmapToDraw) onto another bitmap (dest).
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param bitmapToDraw: The bitmap to be drawn onto the destination
	///	@param x,y:					The x,y location to draw the bitmap to
	///
	/// Side Effects:
	///	- Draws the bitmapToDraw at the x,y location in the destination.
	procedure DrawBitmap(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
    overload;
	var
		offset: SDL_Rect;
	begin
		offset := NewSDLRect(x, y, 0, 0);
		SDL_BlitSurface(bitmapToDraw.surface, nil, dest.surface, @offset);
	end;

	/// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest).
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param bitmapToDraw: The bitmap to be drawn onto the destination
	///	@param srcX, srcY:	 The x,y offset to the area to copy in bitmapToDraw
	///	@param srcW, srcH:	 The width and height of the area to copy
	///	@param x,y:					The x,y location to draw the bitmap part to
	///
	/// Side Effects:
	///	- Draws part of the bitmapToDraw at the x,y location in the destination.
  procedure DrawBitmapPart(dest: Bitmap; bitmapToDraw: Bitmap;
													 srcX, srcY, srcW, srcH, x, y : Integer); overload;
	var
		offset, source: SDL_Rect;
	begin
		offset := NewSDLRect(x, y, 0, 0);
		source := NewSDLRect(srcX, srcY, srcW, srcH);

		SDL_BlitSurface(bitmapToDraw.surface, @source, dest.surface, @offset);
	end;

	/// Draws part of a bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw: The bitmap to be drawn onto the screen
	///	@param srcX, srcY:	 The x,y offset to the area to copy in bitmapToDraw
	///	@param srcW, srcH:	 The width and height of the area to copy
	///	@param x,y:					The x,y location to draw the bitmap part to
	///
	/// Side Effects:
	///	- Draws part of the bitmapToDraw at the x,y location on the screen.
	procedure DrawBitmapPart(bitmapToDraw : Bitmap; 
               srcX, srcY, srcW, srcH, x, y : Integer); overload;
	begin
		DrawBitmapPart(scr, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
	end;

	/// Draws one bitmap (bitmapToDraw) onto the screen.
	///
	///	@param bitmapToDraw: The bitmap to be drawn onto the screen
	///	@param x,y:					The x,y location to draw the bitmap to
	///
	/// Side Effects:
	///	- Draws the bitmapToDraw at the x,y location on the screen.
	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Integer); overload;
	begin
		DrawBitmap(scr, bitmapToDraw, x, y);
	end;

	/// Draws the sprite to the screen within a given view port.
	///
	///	@param spriteToDraw:		 The sprite to be drawn
	///	@param vwPrtX, vwPrty: The x, y of the current view port (i.e. screen)
	///	@param vwPrtWidth, vwPrtHeight:		The height and width of the view port
	///
	/// Side Effects:
	///	- The sprite is drawn to the screen, if within view port
	procedure DrawSprite(spriteToDraw : Sprite; 
               vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight : Integer); overload;
	begin
		//Don't draw if its offscreen
		if IsSpriteOffscreen(spriteToDraw, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight)
			 and (vwPrtWidth <> 0)
			 and (vwPrtHeight <> 0) then exit;

		DrawBitmap(spriteToDraw.bitmaps[spriteToDraw.currentFrame],
							 Trunc(spriteToDraw.xPos) - vwPrtX,
							 Trunc(spriteToDraw.yPos) - vwPrtY);
	end;

	/// Draws a sprite to the screen, without using a view port.
	///
	///	@param spriteToDraw:		 The sprite to be drawn
	///
	/// Side Effects:
	///	- The sprite is drawn to the screen, if within screen area
	procedure DrawSprite(spriteToDraw : Sprite); overload;
	begin
		DrawSprite(spriteToDraw, 0, 0, 0, 0);
	end;

	/// Determines if a sprite is off the screen. The view port of the screen
	///	is defined in the vwPrt... parameters.
	///
	///	@param theSprite:			The sprite to check the position of
	///	@param vwPrtX, vwPrty: The x, y of the current view port (i.e. screen)
	///	@param vwPrtWidth, vwPrtHeight:		The height and width of the view port
	///	@returns							 True if the sprite is off the screen
	function IsSpriteOffscreen(theSprite : Sprite; 
              vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight : Integer): Boolean;
	var
		bmp: Bitmap;
	begin
		bmp := theSprite.bitmaps[theSprite.currentFrame];
		if theSprite.xPos > vwPrtX + vwPrtWidth then result := true
		else if theSprite.xPos + bmp.width < vwPrtX then result := true
		else if theSprite.yPos > vwPrtY + vwPrtHeight then result := true
		else if theSprite.yPos + theSprite.bitmaps[theSprite.currentFrame].height 
               < vwPrtY then result := true
		else result := false;
	end;

  /// Determines if a sprite is off the screen.
	///
	///	@param theSprite:			The sprite to check the position of
	///	@returns							 True if the sprite is off the screen
  function IsSpriteOffscreen(theSprite : Sprite): Boolean;
  begin
    result := IsSpriteOffscreen(theSprite, 0, 0, scr.width, scr.height);
  end;

	/// Moves a sprite based on information in a movement vector.
	///
	///	@param spriteToMove:		 The sprite to move
	///	@param movementVector:	 The vector containing the movement details
	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector);
	begin
		spriteToMove.xPos := spriteToMove.xPos + movementVector.x;
		spriteToMove.yPos := spriteToMove.yPos + movementVector.y;
	end;

	/// Moves a sprite to a given x,y location.
	///
	///	@param spriteToMove:		 the sprite being moved
	///	@param x, y:						 the new location of the sprite
	///
	/// Side Effects:
	///	- Moves the sprite, changing its x and y
	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer);
	begin
		spriteToMove.xPos := x;
		spriteToMove.yPos := y;
	end;

	/// Creates a new vector with values x and y, possibly with an inverted y. The
	///	inversion of the y value provides a convienient option for handling
	///	screen related vectors.
	///
	///	@param x, y			Initial values for the vector
	///	@param invertY	 Indicates if the y value should be inverted.
	///	@returns				 A new vector with values x and y
	function CreateVector(x,y : Single; invertY : boolean): Vector; overload;
	begin
		if invertY then y := y * -1;

		result.x := x;
		result.y := y;
		result.w := 1;
	end;

	/// Creates a new vector with values x and y.
	///
	///	@param x, y			Initial values for the vector
	///	@returns				 A new vector with values x and y
	function CreateVector(x,y : Single): Vector; overload;
	begin
		result := CreateVector(x,y,false);
	end;


	/// Adds v1 and v2.
	///
	///	@param v1, v2			The vectors to work with
	///	@returns					 v1 + v2
	function AddVectors(v1, v2 : Vector): Vector;
	begin
		result.x := v1.x + v2.x;
		result.y := v1.y + v2.y;
	end;

	/// Subtracts v2 from v1 (i.e. v1 - v2).
	///
	///	@param v1, v2			The vectors to work with
	///	@returns					 v1 - v2
	function SubtractVectors(v1, v2 : Vector): Vector;
	begin
		result.x := v1.x - v2.x;
		result.y := v1.y - v2.y;
	end;

	/// Inverts the vector v. Changes the direction of the vector's x and y.
	///
	///	@param v		 The vector to invert
	///	@returns		 A new inverted vector
	function InvertVector(v : Vector): Vector;
	begin
		result.x := v.x * -1;
		result.y := v.y * -1;
	end;

	/// Limits the vector within the range min-max of X-Y. AVOID use... use
  ///  LimitMagnitude
	///
	///	@param theVector		 The vector to limit
	///	@param minX, maxX		The limited range of the vector's x
	///	@param minY, maxY		The limited range of the vector's y
	///	@returns						 A new vector limited within that range
	function ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer)
    : Vector;
	begin
		if theVector.x > maxX then theVector.x := maxX;
		if theVector.x < minX then theVector.x := minX;
		if theVector.y > maxY then theVector.y := maxY;
		if theVector.y < minY then theVector.y := minY;

		result := theVector;
	end;

	/// Limit the magnitude of a vector.
	///
	///	@param theVector:		The vector to limit
	///	@param maxMagnitude: The maximum magnitude of the vector.
	///	@returns						 A new vector with the same direction as theVector,
	///											 with a maximum magnitude of maxMagnitude
	function LimitVector(theVector: Vector; maxMagnitude: Single): Vector;
	var
		magnitude: Single;
	begin
		magnitude := GetVectorMagnitude(theVector);

		if magnitude > maxMagnitude then
		begin
			result := Multiply(ScaleMatrix(maxMagnitude), GetUnitVector(theVector));
		end
		else
		begin
			result := theVector;
		end;
	end;

	/// Gets the unit vector of the passed in vector. The unit vector has a
	///	magnitude of 1, resulting in a vector that indicates the direction of
	///	the original vector.
	///
	///	@param theVector		 The vector to get the unit vector of
	///	@returns						 The unit vector from the passed in vector
	function GetUnitVector(theVector : Vector): Vector;
	var
		temp : Double;
		vectorMagnitude : Single;
	begin
		vectorMagnitude := GetVectorMagnitude(theVector);

		if vectorMagnitude = 0 then
			temp := 0
		else
			temp := 1 / GetVectorMagnitude(theVector);

		result.x := temp * theVector.x;
		result.y := temp * theVector.y;
	end;

	/// Indicates if the magnitude of the vector is 0.
	///
	///	@param theVector		 The vector to check
	///	@returns						 True if the vector has values 0, 0
	function IsZeroVector(theVector : Vector): Boolean;
	begin
		result := (theVector.x = 0) and (theVector.y = 0);
	end;

	/// Returns the magnitude of a vector. The magnitude represents the length of
	///	the vector.
	///
	///	@param theVector			 The vector to get the magnitude of
	///	@returns							 The magnitude of the vector
	function GetVectorMagnitude(theVector : Vector): Single;
	begin
		result := Sqrt((theVector.x * theVector.x) + (theVector.y * theVector.y));
	end;

	/// Decay a vector using information in the decayProps. This can be used to
	///	decay for gravety, and to allow smooth stopping/friction.
	///
	///	@param theVector		 the vector to decay
	///	@param decayProps		The properties used to perform the decay
	///	@returns						 A new vector created as a result
	function DecayVector(theVector : Vector; const decayProps : DecayProperties)
            : Vector;
	begin
		if theVector.x > 0 then
		begin
			theVector.x := theVector.x - decayProps.posXDecay;
			if theVector.x < 0 then theVector.x := 0;
		end
		else if theVector.x < 0 then
		begin
			theVector.x := theVector.x + decayProps.negXDecay;
			if theVector.x > 0 then theVector.x := 0;
		end;

		if theVector.y > 0 then
		begin
			theVector.y := theVector.y - decayProps.posYDecay;
			if theVector.y < 0 then theVector.y := 0;
		end
		else if theVector.y < 0 then
		begin
			theVector.y := theVector.y + decayProps.negYDecay;
			if theVector.y > 0 then theVector.y := 0;
		end;

		result.x := theVector.x;
		result.y := theVector.y;
	end;

	function DotProduct(v1, v2: Vector): Single;
	begin
		result := (v1.x * v2.x) + (v1.y * v2.y);
	end;
	
	function MultiplyVector(v1: Vector; s1: Single): Vector;
	begin
		result.x := v1.x * s1;
		result.y := v1.y * s1;
	end;

	/// Determines if a sprite has collided with a given x position.
	///
	///	@param theSprite:		The sprite to check
	///	@param x:						The x location to check collision with
	///	@param range:				The kind of check to perform less, larger or equal.
	///
	///	@returns						 True if the sprite is within the range requested
	function HasSpriteCollidedX(theSprite: Sprite; x: Integer; 
              range: CollisionDetectionRange): Boolean;
	begin
		if range = CollisionRangeEquals then
			result := (x >= theSprite.xPos) and 
         (x <= theSprite.xPos + theSprite.bitmaps[theSprite.currentFrame].width)
		else if range = CollisionRangeGreaterThan then
			result := x <= theSprite.xPos + 
                     theSprite.bitmaps[theSprite.currentFrame].width
		else if range = CollisionRangeLessThan then
			result := x >= theSprite.xPos
		else
			raise Exception.Create('Invalid Collision Range');
	end;

	/// Determines if a sprite has collided with a given y position. The x and y
	///	values are in "world" coordinates.
	///
	///	@param theSprite:		The sprite to check
	///	@param y:						The y location to check collision with
	///	@param range:				The kind of check to perform less, larger or equal.
	///
	///	@returns						 True if the sprite is within the range requested
	function HasSpriteCollidedY(theSprite : Sprite; y : Integer; 
               range : CollisionDetectionRange): Boolean;
	begin
		if range = CollisionRangeEquals then
			result := (y >= theSprite.yPos) and 
        (y <= theSprite.yPos + theSprite.bitmaps[theSprite.currentFrame].height)
		else if range = CollisionRangeGreaterThan then
			result := y <= theSprite.yPos + 
                     theSprite.bitmaps[theSprite.currentFrame].height
		else if range = CollisionRangeLessThan then
			result := y >= theSprite.yPos
		else
			raise Exception.Create('Invalid Collision Range');
	end;

	function HasBitmapCollidedWithRect(const image: Bitmap; 
                  x, y, rectX, rectY, rectWidth, rectHeight: Integer): Boolean;
	begin
		if y + image.height <= rectY then result := false
		else if y >= rectY + rectheight then result := false
		else if x + image.width <= rectX then result := false
		else if x >= rectX + rectWidth then result := false
		else result := true;
	end;

	/// Determined if a sprite has collided with a given rectangle. The rectangles
	///	coordinates are expressed in "world" coordinates.
	///
	///	@param theSprite:			The sprite to check
	///	@param x, y :					The x,y location of the rectangle
	///	@param width, height:	The width and height of the rectangle
	///
	///	@returns							 True if the sprite collides with the rectangle
	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Boolean; overload;
	begin
		if theSprite.yPos + CurrentHeight(theSprite) <= y then result := false
		else if theSprite.yPos >= y + height then result := false
		else if theSprite.xPos + CurrentWidth(theSprite) <= x then result := false
		else if theSprite.xPos >= x + width then result := false
		else result := true;
	end;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height: Integer; vwPrtX, vwPrtY: Integer)
		: Boolean; overload;
	begin
		result := HasSpriteCollidedWithRect(theSprite, 
			x + vwPrtX, y + vwPrtY, width, height);
	end;

	function IsPixelDrawnAtPoint(image: Bitmap; x, y: Integer) : Boolean;
	begin
		result := (Length(image.nonTransparentPixels) = image.width)
							and ((x >= 0) and (x < image.width))
							and ((y >= 0) and (y < image.height))
							and image.nonTransparentPixels[x, y];
	end;

	/// Performs a collision detection within two bitmaps at the given x, y
	///	locations using per pixel collision detection. This checks to see if
	///	two non-transparent pixels collide.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				 The x,y location of image 1
	///	@param x2, y2:				 The x,y location of image 2
	///
	///	@returns							 True if the bitmaps collide.
	///
	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
        : Boolean; overload;
	begin
		result := CollisionWithinBitmapImages(image1, x1, y1, false, 
                  image2, x2, y2, false);
	end;

	/// Performs a collision detection within two bitmaps at the given x, y
	///	locations. The bounded values indicate if each bitmap should use per
	///	pixel collision detection or a bounded collision detection. This version
	///	uses pixel based checking at all times.
	///
	///	When both bitmaps are using bounded collision the routine checks to see
	///	if the bitmap rectangles intersect. If one is bounded and the other is
	///	pixel based the routine checks to see if a non-transparent pixel in the
	///	pixel based image intersects with the bounds of the bounded image. If
	///	both are pixel based, the routine checks to see if two non-transparent
	///	pixels collide.
	///
	///	Note: Bitmaps do not need to actually be drawn on the screen.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				 The x,y location of image 1
	///	@param bounded1:			 Indicates if image1 should use bounded collision
	///	@param x2, y2:				 The x,y location of image 2
	///	@param bounded2:			 Indicates if image2 should use bounded collision
	///
	///	@returns							 True if the bitmaps collide.
	///
	function CollisionWithinBitmapImages(
            image1: Bitmap; x1, y1: Integer; bounded1: Boolean;
						image2: Bitmap; x2, y2: Integer; bounded2: Boolean)
            : Boolean; overload;
	var
		left1, left2, overLeft: Integer;
		right1, right2, overRight: Integer;
		top1, top2, overTop: Integer;
		bottom1, bottom2, overBottom: Integer;
		i, j, xPixel1, yPixel1, xPixel2, yPixel2: Integer;
	begin
		result := false;

		left1 := x1;
		right1 := x1 + image1.width - 1;
		top1 := y1;
		bottom1 := y1 + image1.height - 1;

		left2 := x2;
		right2 := x2 + image2.width - 1;
		top2 := y2;
		bottom2 := y2 + image2.height - 1;

		if bottom1 > bottom2 then overBottom := bottom2
		else overBottom := bottom1;

		if top1 < top2 then overTop := top2
		else overTop := top1;

		if right1 > right2 then overRight := right2
		else overRight := right1;

		if left1 < left2 then overLeft := left2
		else overLeft := left1;

		for i := overTop to overBottom do
		begin
			yPixel1 := i - top1;
			yPixel2 := i - top2;

			for j := overLeft to overRight do
			begin
				xPixel1 := j - left1;
				xPixel2 := j - left2;

				if (bounded1 or IsPixelDrawnAtPoint(image1, xPixel1, yPixel1))
					 AND (bounded2 or IsPixelDrawnAtPoint(image2, xPixel2, yPixel2)) then
				begin
					result := true;
					exit;
				end;
			end;
		end;
	end;

	function CollisionWithinSpriteImages(sprite1, sprite2: Sprite): Boolean;
	begin
		result := CollisionWithinBitmapImages(sprite1.bitmaps[sprite1.currentFrame],
																					Trunc(sprite1.xPos),
																					Trunc(sprite1.yPos),
																					not sprite1.usePixelCollision,
																					sprite2.bitmaps[sprite2.currentFrame],
																					Trunc(sprite2.xPos),
																					Trunc(sprite2.yPos),
																					not sprite2.usePixelCollision);
	end;

	/// Checks to see if two bitmaps have collided, this performs a bounded check
	///	then, if required, it performs a per pixel check on the colliding region.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				 The x,y location of image 1
	///	@param bounded1:			 Indicates if image1 should use bounded collision
	///	@param x2, y2:				 The x,y location of image 2
	///	@param bounded2:			 Indicates if image2 should use bounded collision
	///
	///	@returns							 True if the bitmaps collide.
	///
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
                           image2 : Bitmap; x2, y2: Integer): Boolean; overload;
	begin
		result := HaveBitmapsCollided(image1, x1, y1, false, image2, x2, y2, false);
	end;

	/// Checks to see if two bitmaps have collided, this performs a bounded check
	///	then, if required, it performs a per pixel check on the colliding region.
	///
	///	@param image1, image2: The bitmap images to check for collision
	///	@param x1, y1:				 The x,y location of image 1
	///	@param bounded1:			 Indicates if image1 should use bounded collision
	///	@param x2, y2:				 The x,y location of image 2
	///	@param bounded2:			 Indicates if image2 should use bounded collision
	///
	///	@returns							 True if the bitmaps collide.
	///
  function HaveBitmapsCollided(image1: Bitmap; x1,y1: Integer;bounded1: Boolean;
 													 image2: Bitmap; x2, y2: Integer; bounded2: Boolean): 
                           Boolean; overload;
	begin
		if not HasBitmapCollidedWithRect(image1, x1, y1, x2, y2, 
                                     image2.width, image2.height) then
		begin
			result := false;
			exit;
		end;

		result := CollisionWithinBitmapImages(image1, x1, y1, bounded1, 
                                          image2, x2, y2, bounded2);
	end;

	/// Determines if two sprites have collided.
	///
	///	@param sprite1, sprite2:	 The two sprites to check.
	///
	///	@returns									 True if the sprites have collided.
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Boolean;
	begin
		if not HasSpriteCollidedWithRect(sprite1, sprite2.xPos, sprite2.yPos, 
             CurrentWidth(sprite2), CurrentHeight(sprite2)) then
		begin
			result := false;
			exit;
		end;

		if sprite1.usePixelCollision or sprite2.usePixelCollision then
		begin
			result := CollisionWithinSpriteImages(sprite1, sprite2);
		end
		else
		begin
			result := true;
		end;
	end;

	/// Determines if a sprite has collided with a bitmap. The x and y values
	///	are expressed in "world" coordinates.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:					 The x,y location of the bitmap
	///	@param bounded:				Indicates if bitmap should use bounded collision
	///
	///	@returns							 True if the bitmap has collided with the sprite.
	///
	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y: Integer): Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, true);
	end;

  /// Determines if a sprite has collided with a bitmap. The x and y values
	///	are expressed in "screen" coordinates, with vwPrtX and vwPrtY storing
	///	the offset from world to screen coordinates.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:					 The x,y location of the bitmap
	///	@param vwPrtX, vwPrtY: The x and y offset of the screen's portal
	///	@param bounded:				Indicates if bitmap should use bounded collision
	///
	///	@returns							 True if the bitmap has collided with the sprite.
	///
	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y: Integer; bounded: Boolean;
																				vwPrtX, vwPrtY: Integer)
																				: Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap, x + vwPrtX,
																					y + vwPrtY, bounded);
	end;

	/// Determines if a sprite has collided with a bitmap using pixel level
	///	collision detection with the bitmap.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:					 The x,y location of the bitmap
	///
	///	@returns							 True if the bitmap has collided with the sprite.
	///
	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y: Integer; bounded: Boolean)
																				: Boolean; overload;
	begin
		result := CollisionWithinBitmapImages(
                                     theSprite.bitmaps[theSprite.currentFrame],
																					Trunc(theSprite.xPos),
																					Trunc(theSprite.yPos),
																					not theSprite.usePixelCollision,
																					theBitmap,
																					x, y, bounded);
	end;

	/// Determines if a sprite has collided with a bitmap. The x and y values
	///	are expressed in "screen" coordinates, with vwPrtX and vwPrtY storing
	///	the offset from world to screen coordinates.
	///
	///	@param theSprite:			The sprite to check for collision
	///	@param theBitmap:			The bitmap image to check for collision
	///	@param x, y:					 The x,y location of the bitmap
	///	@param vwPrtX, vwPrtY: The x and y offset of the screen's portal
	///
	///	@returns							 True if the bitmap has collided with the sprite.
	///
	function	HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
																				x, y, vwPrtX, vwPrtY: Integer)
																				: Boolean; overload;
	begin
		result := HasSpriteCollidedWithBitmap(theSprite, theBitmap,
																					x + vwPrtX, y + vwPrtY);
	end;

	/// Returns true when a key is typed. This occurs when the key is pressed on the 
	/// keyboard, and will not reoccur until it is released and pressed again. This
	/// needs to be checked each ProcessEvents loop.
	///
	/// @param:	virtKeyCode			the code of the key to check
	/// @returns:					True if the key is pressed
	function WasKeyTyped(virtKeyCode: Integer): Boolean;
	begin
		result := sdlManager.WasKeyTyped(virtKeyCode);
	end;

	/// Returns true when the key requested is being held down. This is updated
	///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
	///	unit.
	///
	///	@returns:	 True if the key is currently being held down
	function IsKeyPressed(virtKeyCode : Integer): Boolean;
	var
		keys: PUint8;
		indexAddress: Integer;
		intPtr: ^UInt8;
	begin
		keys := SDL_GetKeyState(nil);

		if keys <> nil then
		begin
			indexAddress := Integer(keys) + (virtKeyCode);

			{$IFDEF FPC}
				intPtr := PUInt8(indexAddress);
			{$ELSE}
				intPtr := Ptr(indexAddress);
			{$ENDIF}
			result := intPtr^ = 1;
		end
		else
		begin
			result := false;
		end;
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

	/// Creates a bitmap in memory that can be drawn onto. The bitmap is initially
	///	transparent and can be used as the target for various drawing operations.
	///	Once you have drawn the desired image onto the bitmap you can call
	///	OptimiseBitmap to optimise the surface.
  ///
  ///  @param width, height:  The width and height of the surface
  ///  @returns:              A new bitmap
	function	CreateBitmap(width, height: Integer): Bitmap;
	begin
		New(result);

		with baseSurface.format^ do
		begin
			result.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32,
											 RMask, GMask, BMask, AMask);
		end;
		result.width := width;
		result.height := height;
		SDL_SetAlpha(result.surface, SDL_SRCALPHA, SDL_ALPHA_OPAQUE);
		SDL_FillRect(result.surface, nil, ColorTransparent);
	end;

	/// Created bitmaps can be optimised for faster drawing to the screen. This
	///	optimisation should be called only once after all drawing to the bitmap
	///	is complete. Optimisation should not be used if the bitmap is to be
	///	drawn to in the future. All loaded bitmaps are optimised during loading.
	///
	///	@param surface:	The bitmap to be optimised
	///
	/// Side Effects:
	///	- Bitmap is optimised, and should not be used to draw onto in the future
	procedure OptimiseBitmap(surface: Bitmap);
	var
		oldSurface: PSDL_Surface;
	begin
		oldSurface := surface.surface;
		SetNonAlphaPixels(surface, oldSurface);
		surface.surface := SDL_DisplayFormatAlpha(oldSurface);
		SDL_FreeSurface(oldSurface);
	end;
	
	/// Loads a font from file with the specified side. Fonts must be freed using
	///	the FreeFont routine once finished with. Once the font is loaded you
	///	can set its style using SetFontStyle. Fonts are then used to draw and
	///	measure text in your programs.
	///
	///	@param fontName: The name of the font file to load from the file system
	///	@param size:		 The point size of the font
	///	@returns:				The font loaded
	function LoadFont(fontName: String; size: Integer): Font;
	begin
		result := TTF_OpenFont(PChar(fontName), size);
		if result = nil then
		begin
			raise Exception.Create('Unable to CreateFont: ' + TTF_GetError());
		end;
	end;

	/// Sets the style of the passed in font. This is time consuming, so load
	///	fonts multiple times and set the style for each if needed.
	///
	///	@param font:	 The font to set the style of
	///	@param style:	the new style for the font, values can be ORed together
	///
	/// Side Effects:
	///	- The font's style is changed
	procedure SetFontStyle(font: Font; style: FontStyle);
	begin
		TTF_SetFontStyle(font, Integer(style));
	end;

	/// Free a loaded font.
	///
	/// Side Effects:
	///	- The font is freed and cannot be used any more
	procedure FreeFont(var fontToFree: Font);
	begin
		TTF_CloseFont(fontToFree);
		fontToFree := nil;
	end;

{ This function prints "str" with font "font" and color "clrFg"
 * onto a rectangle of color "clrBg".
 * It does not pad the text.
 * If CREATE_SURFACE is NOT passed, the function returns NULL,
 * otherwise, it returns an SDL_Surface * pointer.
}
function PrintStrings(dest: PSDL_Surface; font: Font; str: String; 
         rc: PSDL_Rect; clrFg, clrBg:Color; flags:FontAlignment) : PSDL_Surface;
var
	sText: Bitmap;
	temp: PSDL_Surface;
	lineSkip, width, height: Integer;
	lines: Array of String;
	subStr: String;
	n, w, h, i: Integer;
	rect: TSDL_Rect;
begin
	result := nil;

	// If there's nothing to draw, return NULL
	if (Length(str) = 0) or (font = nil) then exit;

	// This is the surface that everything is printed to.
	lineSkip	:= TTF_FontLineSkip( font );
	width		 := rc.w;
	height		:= 10;
	SetLength(lines, 1);

	// Break the String into its lines:
	n := -1; i := 0;
	while n <> 0 do
	begin
		// Get until either "\n" or "\0":
		n := Pos(eol, str);

		//Copy all except EOL
		if n = 0 then subStr := str
		else subStr := Copy(str, 1, n - 1);

		//Remove the line from the original string
		if n <> 0 then
		begin
			str := Copy( str, n + Length(eol), Length(str)); //excess not copied...
		end;

		i := i + 1;
		SetLength(lines, i);
		lines[i - 1] := subStr;

		w := 0;
		// Get the size of the rendered text.
		TTF_SizeText(font, PChar(subStr), w, height);
		if w > width then width := w;
	end;

	// Length(lines) = Number of Lines.
	// we assume that height is the same for all lines.
	height := (Length(lines) - 1) * lineSkip + height;

	if dest = nil then
	begin
		raise Exception.Create('Error Printing Strings: There was no surface.');
	end;

	sText := CreateBitmap(width, height);
	//ClearSurface(sText, clrBg);

	// Actually render the text:
	for i := 0 to High(lines) do
	begin
		// The rendered text:
		temp := TTF_RenderText_Blended( font, PChar(lines[i]), ToSDLColor(clrFg));

		// Put it on the surface:
		if IsSet(flags, AlignLeft) or
			 (not (IsSet(flags, AlignCenter) or
						 IsSet(flags, AlignRight))) then
		begin
			// If it's specifically LEFT or none of the others:
			rect := NewSDLRect(0,i*lineSkip,0,0);
		end
		else if IsSet(flags, AlignCenter) then
		begin
			w := 0;
			h := 0;

			TTF_SizeText(font, PChar(lines[i]), w, h);
			rect := NewSDLRect(width div 2 - w div 2, i * lineSkip, 0, 0);
		end
		else if IsSet(flags, AlignRight) then
		begin
			w := 0;
			h := 0;

			TTF_SizeText(font, PChar(lines[i]), w, h);
			rect := NewSDLRect(width - w, i * lineSkip, 0, 0);
		end
		else
		begin
			raise Exception.Create('Invalid font alignment');
		end;

		// Render the current line. Ignore alpha in this draw
		SDL_SetAlpha(temp, 0, SDL_ALPHA_TRANSPARENT);
		SDL_BlitSurface(temp, nil, sText.surface, @rect);

		// Clean up:
		SDL_FreeSurface(temp);
	end;

	// Draw the text on top of that:
	SDL_BlitSurface(sText.surface, nil, dest, rc );
	FreeBitmap(sText);

	result := nil;
end;

	/// Draws texts to the destination bitmap. Drawing text is a slow operation,
	///	and drawing it to a bitmap, then drawing the bitmap to screen is a
	///	good idea. Do not use this technique if the text changes frequently.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param theFont:			The font used to draw the text
	///	@param x,y:					The x,y location to draw the text at (top left)
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		in the destination bitmap.
	procedure DrawText(dest: Bitmap; theText: String; textColor: Colour;
                     theFont: Font; x, y: Integer); overload;
	var
		color: TSDL_Color;
		surface: PSDL_Surface;
		offset: TSDL_Rect;
	begin
		color := ToSDLColor(textColor);

		offset.x := x;
		offset.y := y;

		surface := TTF_RenderText_Blended(theFont, PChar(theText), color);

		SDL_BlitSurface(surface, nil, dest.surface, @offset);
		SDL_FreeSurface(surface);
	end;

	/// Draws multiple lines of text to the destination bitmap. This is a very
	///	slow operation, so if the text is not frequently changing save it to a
	///	bitmap and draw that bitmap to screen instead.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param backColor:		The color to draw behind the text
	///	@param theFont:			The font used to draw the text
	///	@param align:				The alignment for the text in the region
	///	@param x,y:					The x,y location to draw the text at (top left)
	///	@param w, h:				 The width and height of the region to draw inside
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		in the destination bitmap.
	procedure DrawTextLines(dest: Bitmap; theText: String;
													textColor, backColor: Colour;
													theFont: Font; align: FontAlignment;
													x, y, w, h: Integer); overload;
	var
		rect: TSDL_Rect;
	begin
		rect := NewSDLRect(x, y, w, h);
		PrintStrings(dest.surface, theFont, theText, @rect,
								 textColor, backColor, align);
	end;

	/// Draws multiple lines of text to the screen. This is a very
	///	slow operation, so if the text is not frequently changing save it to a
	///	bitmap and draw that bitmap to screen instead.
	///
	///	@param theText:			The text to be drawn onto the destination
	///	@param textColor:		The color to draw the text
	///	@param backColor:		The color to draw behind the text
	///	@param theFont:			The font used to draw the text
	///	@param align:				The alignment for the text in the region
	///	@param x,y:					The x,y location to draw the text at (top left)
	///	@param w, h:				 The width and height of the region to draw inside
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		on the screen.
	procedure DrawTextLines(theText: String; textColor, backColor: Colour;
													theFont: Font; align: FontAlignment;
													x, y, w, h: Integer); overload;
	begin
		DrawTextLines(scr, theText, textColor, backColor, theFont, align, 
                  x, y, w, h);
	end;

	/// Draws texts to the screen. Drawing text is a slow operation,
	///	and drawing it to a bitmap, then drawing the bitmap to screen is a
	///	good idea. Do not use this technique if the text changes frequently.
	///
	///	@param theText:			The text to be drawn onto the screen
	///	@param textColor:		The color to draw the text
	///	@param theFont:			The font used to draw the text
	///	@param x,y:					The x,y location to draw the text at (top left)
	///
	/// Side Effects:
	///	- The text is drawn in the specified font, at the indicated location
	///		on the screen.
	procedure DrawText(theText: String; textColor: Colour; theFont: Font;
                     x, y: Integer);
	begin
		DrawText(scr, theText, textColor, theFont, x, y);
	end;

  /// StartReadingText start the API reading a string values from the user.
	///	Entry is completed when the user presses enter, and aborted with escape.
	///	If the user aborts entry the result is an empty string. Text entry is
	///	updated as part of ProcessEvents, and is drawn to the screen as part of
	///	the RefreshScreen call.
	///
	///	@param textColor:	The color of the text entered by the user
	///	@param maxLength:	The maximum length of the string the user can enter
	///	@param theFont:		The font used to draw the text entered
	///	@param x, y:			 The location at which to draw the text entered
	procedure StartReadingText(textColor: Colour; maxLength: Integer; 
                             theFont: Font; x, y: Integer);
	begin
		sdlManager.StartReadingText(ToSDLColor(textColor), maxLength, theFont, x, y);
	end;

  /// IsReadingText indicates if the API is currently reading text from the
	///	user. Calling StartReadingText will set this to true, and it becomes
	///	false when the user presses enter or escape. At this point you can
	///	read the string entered as either ASCII or Unicode.
	///
	///	@returns: True while the API is reading text from the user
	function	IsReadingText(): Boolean;
	begin
		result := sdlManager.IsReading;
	end;

 	/// TextReadAsASCII allows you to read the value of the string entered by the
	///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
	///	for more details.
	///
	///	@return:	 The string entered by the user
	function	TextReadAsASCII(): String;
	begin
		result := String(sdlManager.EnteredString);
	end;

  /// TextReadAsUNICODE returns the string entered by the user as UNICODE. See
	///	TextReadAsASCII, StartReadingText, and IsReadingText for more details.
	///
	///	@returns:	The string entered by the user
	function	TextReadAsUNICODE(): WideString;
	begin
		result := sdlManager.EnteredString;
	end;

	/// Calculates the width of a string when drawn with a given font.
	///
	///	@param theText:		The text to measure
	///	@param theFont:		The font used to draw the text
	///	@returns					 The width of the drawing in pixels
	function TextWidth(theText: String; theFont: Font): Integer;
	var
		y: Integer; //SizeText returns both... store and ignore y
	begin
		TTF_SizeText(theFont, PChar(theText), result, y);
	end;

	/// Calculates the height of a string when drawn with a given font.
	///
	///	@param theText:		The text to measure
	///	@param theFont:		The font used to draw the text
	///	@returns					 The height of the drawing in pixels
	function TextHeight(theText: String; theFont: Font): Integer;
	var
		w: Integer; //SizeText returns both... store and ignore w
	begin
		TTF_SizeText(theFont, PChar(theText), w, result);
	end;

	/// Draws a pixel onto the screen.
	///
	///	@param theColor:		 The color to draw the pixel
	///	@param x,y:					The x,y location to draw the pixel at
	///
	/// Side Effects:
	///	- Sets one pixel on the screen
	procedure DrawPixel(theColour: Colour; x, y: Integer); overload;
	begin
		DrawPixel(scr, theColour, x, y);
	end;

	/// Draws a rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param filled:			 True to draw a filled rectangle, false for outline
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure DrawRectangle(theColour : Colour; filled : Boolean;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, filled, xPos, yPos, width, height);
	end;

	/// Draws the outline of a rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure DrawRectangle(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		DrawRectangle(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a filled rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle on the screen
	procedure FillRectangle(theColour : Colour;
                          xPos, yPos, width, height : Integer); overload;
	begin
		FillRectangle(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param xPosStart,yPosStart: The x,y location to start the line at
	///	@param xPosEnd, yPosEnd:		The x,y location to end the line at
	///
	/// Side Effects:
	///	- Draws a line in the screen
	procedure DrawLine(theColour: Colour; 
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Integer); overload;
	begin
		DrawLine(scr, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
	end;

	/// Draws a horizontal line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param y:						The y location of the line
	///	@param x1, x2:			 The starting and ending x value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Integer); overload;
	begin
		DrawHorizontalLine(scr, theColor, y, x1, x2);
	end;

	/// Draws a vertical line on the screen.
	///
	///	@param theColor:		 The color to draw the line
	///	@param x:						The x location of the line
	///	@param y1, y2:			 The starting and ending y value of the line
	///
	/// Side Effects:
	///	- Draws a line on the screen
	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Integer); overload;
	begin
		DrawVerticalLine(scr, theColor, x, y1, y2);
	end;

	/// Draws a circle centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param filled:			 True to draw a filled circle, false for outline
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure DrawCircle(theColour: Colour; filled: Boolean;
                       xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, filled, xc, yc, radius);
	end;

	/// Draws a circle outline centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure DrawCircle(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		DrawCircle(scr, theColour, xc, yc, radius);
	end;

	/// Draws a filled circle centered on a given x, y location.
	///
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle on the screen
	procedure FillCircle(theColour: Colour; xc, yc, radius: Integer); overload;
	begin
		FillCircle(scr, theColour, xc, yc, radius);
	end;

	/// Draws a ellipse within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param filled:			 True to draw a filled ellipse, false for outline
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse on the screen
	procedure DrawEllipse(theColour: Colour; filled: Boolean;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, filled, xPos, yPos, width, height);
	end;

	/// Draws a ellipse outline within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse on the screen
	procedure DrawEllipse(theColour: Colour;
                        xPos, yPos, width, height: Integer); overload;
	begin
		DrawEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a filled ellipse within a given rectangle on the screen.
	///
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the screen
	 procedure FillEllipse(theColour: Colour; 
                         xPos, yPos, width, height: Integer); overload;
	begin
		FillEllipse(scr, theColour, xPos, yPos, width, height);
	end;

	/// Draws a rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param filled:			 True to draw a filled rectangle, false for outline
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure DrawRectangle(dest: Bitmap; theColour : Colour; filled : Boolean;
                          xPos, yPos, width, height : Integer); overload;
	begin
		if filled then
		begin
			FillRectangle(dest, theColour, xPos, yPos, width, height);
		end
		else
		begin
			DrawRectangle(dest, theColour, xPos, yPos, width, height);
		end;
	end;

	/// Draws the outline of a rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	 procedure DrawRectangle(dest: Bitmap; theColour : Colour;
                           xPos, yPos, width, height : Integer); overload;
	begin
		DrawHorizontalLine(dest, theColour, yPos, xPos, xPos + width);
		DrawHorizontalLine(dest, theColour, yPos + height, xPos, xPos + width);
		DrawVerticalLine(dest, theColour, xPos, yPos, yPos + height);
		DrawVerticalLine(dest, theColour, xPos + width, yPos, yPos + height);
	end;

	/// Draws a filled rectangle on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the rectangle
	///	@param xPos,yPos:		The x,y location to draw the rectangle at
	///	@param width,height: The width and height of the rectangle
	///
	/// Side Effects:
	///	- Draws a rectangle in the dest bitmap
	procedure FillRectangle(dest: Bitmap; theColour : Colour; 
                          xPos, yPos, width, height : Integer);
	var
		rect: SDL_Rect;
	begin
		if width < 0 then
		begin
			rect.x := xPos + width; //move back by width
			width := -width;
		end
		else rect.x := xPos;

		if height < 0 then
		begin
			rect.y := yPos + height; //move up by height
			height := -height;
		end
		else rect.y := yPos;

		rect.w := width;
		rect.h := height;

		SDL_FillRect(dest.surface, @rect, theColour);
	end;

	/// Draws a ellipse within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param filled:			 True to draw a filled ellipse, false for outline
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	procedure DrawEllipse(dest: Bitmap; theColour: Colour; filled: Boolean;
                        xPos, yPos, width, height: Integer); overload;
	begin
		if filled then
		begin
			FillEllipse(dest, theColour, xPos, yPos, width, height);
		end
		else
		begin
			DrawEllipse(dest, theColour, xPos, yPos, width, height);
		end;
	end;

	/// Draws a ellipse outline within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	procedure DrawEllipse(dest: Bitmap; theColour: Colour; 
                 xPos, yPos, width, height: Integer); overload;
	var
		xRadius, yRadius: Integer;
	begin
		xRadius := width div 2;
		yRadius := height div 2;

		SwinGameDrawing.DrawEllipse(dest.surface, xPos + xRadius, 
                         yPos + yRadius, xRadius, yRadius, theColour);
	end;

	/// Draws a filled ellipse within a given rectangle on the dest bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the ellipse
	///	@param xPos,yPos:		The x,y location of the top left of the ellipse
	///	@param width,height: The width and height of the ellipse
	///
	/// Side Effects:
	///	- Draws a ellipse in the dest bitmap
	 procedure FillEllipse(dest: Bitmap; theColour: Colour;
                         xPos, yPos, width, height: Integer);
	var
		xRadius, yRadius: Integer;
	begin
		xRadius := width div 2;
		yRadius := height div 2;

		SwinGameDrawing.FillEllipse(dest.surface, xPos + xRadius, 
                       yPos + yRadius, xRadius, yRadius, theColour);
	end;

	/// Draws a vertical line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param x:						The x location of the line
	///	@param y1, y2:			 The starting and ending y value of the line
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawVerticalLine(dest: Bitmap; theColor: Color; x, y1, y2: Integer);
	begin
		SwinGameDrawing.DrawVerticalLine(dest.surface, x, y1, y2, theColor);
	end;

	/// Draws a horizontal line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param y:						The y location of the line
	///	@param x1, x2:			 The starting and ending x value of the line
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawHorizontalLine(dest: Bitmap; theColor: Color; y,x1,x2: Integer);
	begin
		SwinGameDrawing.DrawHorizontalLine(dest.surface, y, x1, x2, theColor);
	end;

	/// Draws a line on the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the line
	///	@param xPosStart,yPosStart: The x,y location to start the line at
	///	@param xPosEnd, yPosEnd:		The x,y location to end the line at
	///
	/// Side Effects:
	///	- Draws a line in the dest bitmap
	procedure DrawLine(dest: Bitmap; theColour: Colour;
                     xPosStart, yPosStart, xPosEnd, yPosEnd: Integer);
	begin
		if xPosStart = xPosEnd then
      DrawVerticalLine(dest, theColour, xPosStart, yPosStart, yPosEnd)
		else if yPosStart = yPosEnd then
      DrawHorizontalLine(dest, theColour, yPosStart, xPosStart, xPosEnd)
		else
      SwinGameDrawing.DrawLine(dest.surface, xPosStart, yPosStart,
                                             xPosEnd, yPosEnd, theColour);
	end;

 	/// Draws a pixel onto the destination bitmap.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the pixel
	///	@param x,y:					The x,y location to draw the pixel at
	///
	/// Side Effects:
	///	- Sets one pixel on the destination bitmap
	procedure DrawPixel(dest: Bitmap; theColour: Colour; x, y: Integer); overload;
	begin
		SwinGameDrawing.DrawPixel(dest.surface, x, y, theColour);
	end;

	/// Draws a circle centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param filled:			 True to draw a filled circle, false for outline
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure DrawCircle(dest: Bitmap; theColour: Colour; filled: Boolean;
                       xc, yc, radius: Integer); overload;
	begin
		if filled then
			SwinGameDrawing.DrawCircle(dest.surface, xc, yc, radius, theColour)
		else
			SwinGameDrawing.FillCircle(dest.surface, xc, yc, radius, theColour);
	end;

	/// Draws a circle outline centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure DrawCircle(dest: Bitmap; theColour: Colour; xc,yc, radius: Integer);
   overload;
	begin
		SwinGameDrawing.DrawCircle(dest.surface, xc, yc, radius, theColour)
	end;

	/// Draws a filled circle centered on a given x, y location.
	///
	///	@param dest:				 The destination bitmap - not optimised!
	///	@param theColor:		 The color to draw the circle
	///	@param xc,yc:				The x,y location of the center of the circle
	///	@param radius:			 The radius of the circle
	///
	/// Side Effects:
	///	- Draws a Circle in the dest bitmap
	procedure FillCircle(dest: Bitmap; theColour: Colour;
                       xc, yc, radius: Integer);
	begin
		SwinGameDrawing.FillCircle(dest.surface, xc, yc, radius, theColour);
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

	/// Draws the frame rate using the specified font at the indicated x, y.
	///	Draws the FPS (min, max) current average
	///
	///	@param x,y:			The x, y location to draw to
	///	@param font:		 The font used to draw the framerate
	///
	/// Side Effects:
	///	- Framerate is drawn to the screen
	procedure DrawFramerate(x, y: integer; font: Font);
	var
		temp, temp2, temp3 : String;
		textColour : Colour;
		average, highest, lowest : Single;
	begin
		//Draw framerates
		DrawRectangle(ColourBlack, true, x, y, x + 200, y + 16);

		if renderFPSInfo.average = 0 then
			average := 9999
		else
			average := (1000 / renderFPSInfo.average);
		
		lowest	:= (1000 / renderFPSInfo.high);
		highest := (1000 / renderFPSInfo.low);

		if average < 30 then
			textColour := ColourRed
		else if average < 50 then
			textColour := ColourYellow
		else
			textColour := ColourGreen;

		Str(average:4:1, temp);
		Str(highest:4:1, temp2);
		Str(lowest:4:1, temp3);

		DrawText('FPS: (' + temp3 + ', ' + temp2 + ') ' + temp, 
             textColour, font, x + 2, y + 2);
	end;

//*******
//
// Sound routines
//
//*******

	/// Loads a sound effect from the file system. The sound effect can be in the
	///	form of a wav, ogg, or mp3 file.
	///
	///	@param path			The path to the file to load
	///	@returns				 The sound effect
  function LoadSoundEffect(path: String): SoundEffect;
  begin
	  result := Mix_LoadWAV(pchar(path));
	  if result = nil then
	  begin
	  	raise Exception.Create('Error loading sound effect: ' + SDL_GetError());
	  end;
  end;

	/// Load music to play from the file system. Music can be in the form of a
	///	wav, ogg, or mp3 file.
	///
	///	@param path			The path to the file to be loaded
	///	@returns				 The loaded music value
  function LoadMusic(path: String): Music;
  begin
    result := Mix_LoadMUS(pchar(path));
    if result = nil then
    begin
      raise Exception.Create('Error loading sound effect: ' + SDL_GetError());
	end;
  end;

	/// Free a sound effect. All loaded sound effects need to be freed.
	///
	///	@param effect		The effect to be freed.
	///
	/// Side Effect:
	///	- Frees the sound effect
  procedure FreeSoundEffect(var effect: SoundEffect);
  begin
  	Mix_FreeChunk(effect);
  	effect := nil;
  end;

	/// Free a music value. All loaded music values need to be freed.
	///
	///	@param mus		 The musi to be freed
	///
	/// Side Effect:
	///	- Music is freed
  procedure FreeMusic(var mus: Music);
  begin
	  Mix_FreeMusic(mus);
   	mus := nil;
  end;

	/// Play the indicated sound effect a number of times.
	///
	///	@param effect		 the effect to play
	///	@param loops			The number of times to play it
  procedure PlaySoundEffect(effect: SoundEffect; loops: Integer); overload;
  var
	  i: Integer;
  begin
	  i := Mix_PlayChannel( -1, effect, loops );
  	if i <> -1 then soundChannels[i] := effect;
  end;

	/// Play the indicated sound effect once.
	///
	///	@param effect		 the effect to play
  procedure PlaySoundEffect(effect: SoundEffect); overload;
  begin
	  PlaySoundEffect(effect, 0);
  end;

	/// Start the indicating music playing. The music will loop the indicated
	///	number of times.
	///
	///	@param mus			 The music to begin to play
	///	@param loops		 The number of times to loop the music
	///
	/// Side Effect:
  procedure PlayMusic(mus: Music; loops: Integer); overload;
  var
  	i: Integer;
  begin
	  i := Mix_PlayMusic(mus, loops);
  	if i <> -1 then soundChannels[i] := mus;
  end;

	/// Start the indicating music playing. The music will continue to play
	///	until stopped.
	///
	///	@param mus			 The music to begin to play
	///
	/// Side Effect:
  procedure PlayMusic(mus: Music); overload;
  begin
	  PlayMusic(mus, -1);
  end;

  function IsSoundPlaying(effect: Pointer): Boolean;
  var
	  i: Integer;
  begin
	  result := false;

  	for i := 0 to High(soundChannels) do
	  begin
		  if soundChannels[i] = effect then
  		begin
	  		result := Mix_Playing(i) <> 0;
		  	break;
  		end;
	  end;
  end;

	/// Determines if a sound effect is playing
	///
	///	@param effect		The sound effect check if playing
	///	@return					True if the sound effect is playing
  function IsSoundEffectPlaying(effect: SoundEffect): Boolean;
  begin
	  result := IsSoundPlaying(effect);
  end;

	/// Determines if the indicated music is playing.
	///
	///	@param mus	 The music to check if playing
	///	@returns		 True if the music is playing
  function IsMusicPlaying(mus: Music): Boolean;
  begin
  	result := IsSoundPlaying(mus);
  end;

 	/// Stop music from playing.
	///
	/// Side Effects:
	///	- Stops the currently playing music
  procedure StopMusic();
  begin
	  Mix_HaltMusic();
  end;

	/// Stop playing sound effects
	///
	///	@param effect:	 The sound effect to be stopped
	///
	/// Side Effects:
	///	- The sound stops playing
  procedure StopSoundEffect(effect: SoundEffect);
  var
	  i: Integer;
  begin
	  for i := 0 to High(soundChannels) do
  	begin
	  	if soundChannels[i] = effect then
		  begin
			  Mix_HaltChannel(i);
  			break;
	  	end;
  	end;
  end;

  /// Multiply two matrix2d. Use this to combine the effects to two
  ///  transformations.
  ///
  /// @param m1, m2:   the two matrix to multiply
  /// @returns:        the result of multiplying these matrix
  function Multiply(const m1, m2: Matrix2D): Matrix2D; overload;
  begin
	  //unwound for performance optimisation
  	result[0, 0] := m1[0, 0] * m2[0, 0] +
	  								m1[0, 1] * m2[1, 0] +
  									m1[0, 2] * m2[2, 0];
	result[0, 1] := m1[0, 0] * m2[0, 1] +
									m1[0, 1] * m2[1, 1] +
									m1[0, 2] * m2[2, 1];
	result[0, 2] := m1[0, 0] * m2[0, 2] +
									m1[0, 1] * m2[1, 2] +
									m1[0, 2] * m2[2, 2];

	result[1, 0] := m1[1, 0] * m2[0, 0] +
									m1[1, 1] * m2[1, 0] +
									m1[1, 2] * m2[2, 0];
	result[1, 1] := m1[1, 0] * m2[0, 1] +
									m1[1, 1] * m2[1, 1] +
									m1[1, 2] * m2[2, 1];
	result[1, 2] := m1[1, 0] * m2[0, 2] +
									m1[1, 1] * m2[1, 2] +
									m1[1, 2] * m2[2, 2];

	result[2, 0] := m1[2, 0] * m2[0, 0] +
									m1[2, 1] * m2[1, 0] +
									m1[2, 2] * m2[2, 0];
	result[2, 1] := m1[2, 0] * m2[0, 1] +
									m1[2, 1] * m2[1, 1] +
									m1[2, 2] * m2[2, 1];
	result[2, 2] := m1[2, 0] * m2[0, 2] +
									m1[2, 1] * m2[1, 2] +
									m1[2, 2] * m2[2, 2];

end;

function Multiply(const m: Matrix2D; const v: Vector): Vector; overload;
begin
	result.x := v.x * m[0, 0] + v.y * m[0,1] + v.w * m[0,2];
	result.y := v.x * m[1, 0] + v.y * m[1,1] + v.w * m[1,2];
	//result.w := v.x * m[2, 0] + v.y * m[2,1] + v.w * m[2,2];
	//w remains as 1
end;

function RotationMatrix(deg: Single): Matrix2D;
var
	rads: Double;
begin
	rads := deg * DEG_TO_RAD;

	result[0, 0] := System.Cos(rads);
	result[0, 1] := System.Sin(rads);
	result[0, 2] := 0;

	result[1, 0] := -System.Sin(rads);
	result[1, 1] := System.Cos(rads);
	result[1, 2] := 0;

	result[2, 0] := 0;
	result[2, 1] := 0;
	result[2, 2] := 1;
end;

function ScaleMatrix(scale: Single): Matrix2D;
begin
	result[0, 0] := scale;
	result[0, 1] := 0;
	result[0, 2] := 0;

	result[1, 0] := 0;
	result[1, 1] := scale;
	result[1, 2] := 0;

	result[2, 0] := 0;
	result[2, 1] := 0;
	result[2, 2] := 1;
end;

function TranslationMatric(dx, dy: Single): Matrix2D;
begin
	result := ScaleMatrix(1);

	result[0, 2] := dx;
	result[1, 2] := dy;
end;

function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
var
	o, a, oa, rads: Single;
begin
	if (x1 = x2) and (y2 < y1) then result := 90
	else if (x1 = x2) and (y2 >= y1) then result := 270
	else if (y1 = y2) and (x2 < x1) then result := 180
	else if (y1 = y2) and (x2 >= x1) then result := 0
	else
	begin
		o := (y2 - y1);
		a := (x1 - x2);
		oa := o / a;
		rads := arctan(oa);
		result := RadToDeg(rads);

		if (x2 < x1) then result := result + 180;
	end;
end;

function CalculateAngle(sprite1, sprite2: Sprite): Single; overload;
var
	cx1, cy1, cx2, cy2: Single;
begin
	cx1 := sprite1.XPos + CurrentWidth(sprite1) / 2;
	cy1 := sprite1.YPos + CurrentHeight(sprite1) / 2;
	cx2 := sprite2.XPos + CurrentWidth(sprite2) / 2;
	cy2 := sprite2.YPos + CurrentHeight(sprite2) / 2;
	
	result := CalculateAngle(cx1, cy1, cx2, cy2);
end;

procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
begin
	sdlManager.RegisterEventProcessor(handle, handle2);
end;

var
	applicationPath: String;   //global variable for optimisation...
	
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

function GetPathToResource(filename: String; kind: ResourceKind)
: String; overload;
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



initialization
begin
	if SDL_Init(SDL_INIT_EVERYTHING) = -1 then
	begin
		raise Exception.Create('Error initialising SDL. ' + string(SDL_GetError));
	end;

	SDL_EnableUNICODE(SDL_ENABLE);

	//Load sound
	if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 1024 ) = -1 then
	begin
		raise Exception.Create('Error openning audio device. ' + 
                           string(Mix_GetError));
	end;

	if TTF_Init() = -1 then
	begin
		raise Exception.Create('Error openning font library. ' + 
                            string(TTF_GetError));
	end;

	sdlManager := TSDLManager.Create();
	applicationPath := ExtractFileDir(ParamStr(0));
end;

finalization
begin
	if sdlManager <> nil then
	begin
		sdlManager.Free();
		sdlManager := nil;
	end;

	if scr <> nil then
		FreeBitmap(scr);

	Mix_CloseAudio();
	TTF_Quit();
	SDL_Quit();
end;

end.
