library SGSDK;

uses SGSDK_Core, SGSDK_Input, SGSDK_Audio, SGSDK_Font, SGSDK_Physics, SGSDK_Graphics,
	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing;

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	procedure ProcessEvents(); cdecl; export;
	begin
		SGSDK_Core.ProcessEvents();
	end;

	procedure OpenGraphicsWindow(caption : String; width : Integer; height : Integer); cdecl; export;
	begin
		SGSDK_Core.OpenGraphicsWindow(caption, width, height);
	end;
	
	function WindowCloseRequested(): Integer; cdecl; export;
	begin
		if SGSDK_Core.WindowCloseRequested() then
		begin
			//WriteLn('Close...');
			result:= -1
		end
		else
		begin
			//WriteLn('Not Close...');
			result:= 0
		end
	end;
	
	procedure SetIcon(iconFilename: String); cdecl; export;
	begin
		SGSDK_Core.SetIcon(iconFilename);
	end;
	
	procedure ChangeScreenSize(width, height: Integer); cdecl; export;
	begin
		SGSDK_Core.ChangeScreenSize(width, height);
	end;
	
	procedure ToggleFullScreen(); cdecl; export;
	begin
		SGSDK_Core.ToggleFullScreen();
	end;
	
	procedure RefreshScreen(); cdecl; export;
	begin
		SGSDK_Core.RefreshScreen();
	end;
	
	procedure TakeScreenshot(basename: String); cdecl; export;
	begin
		SGSDK_Core.TakeScreenshot(basename);
	end;
	
	function  ScreenWidth(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.ScreenWidth();
	end;
	
	function  ScreenHeight(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.ScreenHeight();
	end;
	
	function	ToSDLColor(color: UInt32): TSDL_Color; cdecl; export;
	begin
		result := SGSDK_Core.ToSDLColor(color);
	end;
	
	function	GetColourBitmap(forBitmap: Bitmap; apiColor: Color): Colour; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetColour(forBitmap, apiColor);
	end;
	
	function	GetColourRGBA(red, green, blue, alpha: Byte) : Colour; cdecl; export;
	begin
		result := SGSDK_Core.GetColour(red, green, blue, alpha);
	end;
	
	// function GetColour(red, green, blue : Byte) : Colour;
	// pass 255 into parameter 4
	
	function	GetFramerate(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.GetFramerate();
	end;
	
	function	GetTicks(): UInt32; cdecl; export;
	begin
		result := SGSDK_Core.GetTicks();
	end;
	
	procedure Sleep(time : UInt32); cdecl; export;
	begin
		SGSDK_Core.Sleep(time);
	end;
	
	function GetPathToResourceWithKind(filename: String; kind: ResourceKind) : String; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetPathToResource(filename, kind);
	end;
	
	function GetPathToResource(filename: String): String; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetPathToResource(filename);
	end;
	
	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr); cdecl; export;
	begin
		SGSDK_Core.RegisterEventProcessor(handle, handle2);
	end;
	
	function Cos(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Cos(angle);
	end;
	
	function Sin(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Sin(angle);
	end;
	
	function Tan(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Tan(angle);
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function GetMousePosition(): Vector; cdecl; export;
	begin
		result := SGSDK_Input.GetMousePosition();
	end;
	
	function GetMouseMovement(): Vector; cdecl; export;
	begin
		result := SGSDK_Input.GetMouseMovement();
	end;
	
	function IsMouseDown(button: MouseButton): Integer; cdecl; export;
	begin
		if SGSDK_Input.IsMouseDown(button) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	function IsMouseUp(button: MouseButton): Integer; cdecl; export;
	begin
		if SGSDK_Input.IsMouseUp(button) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	function MouseWasClicked(button: MouseButton): Integer; cdecl; export;
	begin
		if SGSDK_Input.MouseWasClicked(button) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;


	procedure StartReadingText(textColor: Colour; maxLength: Integer;
														 theFont: Font; x, y: Integer); cdecl; export;
	begin
		SGSDK_Input.StartReadingText(textColor, maxLength, theFont, x, y);
	end;

	function IsReadingText(): Integer; cdecl; export;
	begin
		if SGSDK_Input.IsReadingText() then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	function TextReadAsASCII(): String; cdecl; export;
	begin
		result := SGSDK_Input.TextReadAsASCII();
	end;
	
	function TextReadAsUNICODE(): WideString; cdecl; export;
	begin
		result := SGSDK_Input.TextReadAsUNICODE();
	end;
	
	function IsKeyPressed(virtKeyCode : Integer): Integer; cdecl; export;
	begin
		if SGSDK_Input.IsKeyPressed(virtKeyCode) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	function WasKeyTyped(virtKeyCode: Integer): Integer; cdecl; export;
	begin
		if SGSDK_Input.WasKeyTyped(virtKeyCode) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	
	procedure	OpenAudio(); cdecl; export;
	begin
		SGSDK_Audio.OpenAudio();
	end;
	
	procedure	CloseAudio(); cdecl; export;
	begin
		SGSDK_Audio.CloseAudio();
	end;
	
	function	LoadSoundEffect(path: String): SoundEffect; cdecl; export;
	begin
		result := SGSDK_Audio.LoadsoundEffect(path);
	end;
	
	function	LoadMusic(path: String): Music; cdecl; export;
	begin
		result := SGSDK_Audio.LoadMusic(path);
	end;

	procedure FreeMusic(var mus: Music); cdecl; export;
	begin
		SGSDK_Audio.FreeMusic(mus);
	end;

	procedure FreeSoundEffect(var effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.FreeSoundEffect(effect);
	end;

	procedure PlaySoundEffect(effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.PlaySoundEffect(effect);
	end;
	
	procedure PlaySoundEffectLoop(effect: SoundEffect; loops: Integer); cdecl; export;
	begin
		SGSDK_Audio.PlaySoundEffect(effect, loops);
	end;

	procedure PlayMusic(mus: Music; loops: Integer); cdecl; export;
	begin
		SGSDK_Audio.PlayMusic(mus, loops);
	end;

	//procedure PlayMusic1(mus: Music); cdecl; export;
	//begin
		//SGSDK_Audio.PlayMusic(mus);
	//end;

	function IsMusicPlaying(mus: Music): Integer; cdecl; export;
	begin
		if SGSDK_Audio.IsMusicPlaying(mus) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function IsSoundEffectPlaying(effect: SoundEffect): Integer; cdecl; export;
	begin
		if SGSDK_Audio.IsSoundEffectPlaying(effect) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	procedure StopSoundEffect(effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.StopSoundEffect(effect);
	end;

	procedure StopMusic(); cdecl; export;
	begin
		SGSDK_Audio.StopMusic();
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function LoadFont(fontName: String; size: Integer): Font; cdecl; export;
	begin
		result := SGSDK_Font.LoadFont(fontName, size)
	end;

	procedure SetFontStyle(font: Font; style: FontStyle); cdecl; export;
	begin
		SGSDK_Font.SetFontStyle(font, style);
	end;

	procedure FreeFont(var fontToFree: Font); cdecl; export;
	begin
		SGSDK_Font.FreeFont(fontToFree);
	end;
	
	procedure DrawText(theText: String; textColor: Colour;
					 theFont: Font; x, y: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawText(theText, textColor, theFont, x, y);
	end;

	procedure DrawTextLines(theText: String; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawTextLines(theText, textColor, backColor, theFont, align, x, y, w, h);
	end;
							
	procedure DrawText1(dest: Bitmap; theText: String; textColor: Colour;
					theFont: Font; x, y: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawText(dest, theText, textColor, theFont, x, y);
	end;

	procedure DrawTextLines1(dest: Bitmap; theText: String;
							textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawTextLines(dest, theText, textColor, backColor, theFont, align, x, y, w, h);
	end;

	function TextWidth(theText: String; theFont: Font): Integer; cdecl; export;
	begin
		result :=SGSDK_Font.TextWidth(theText, theFont);
	end;

	function TextHeight(theText: String; theFont: Font): Integer; cdecl; export;
	begin
		result := SGSDK_Font.TextHeight(theText, theFont);
	end;

	procedure DrawFramerate(x, y: Integer; font: Font); cdecl; export;
	begin
		SGSDK_Font.DrawFramerate(x, y, font);
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function	HasSpriteCollidedX(theSprite : Sprite; x : Integer;
								 range : CollisionDetectionRange):  Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedX(theSprite, x, range) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HasSpriteCollidedY(theSprite : Sprite; y : Integer;
								 range : CollisionDetectionRange): Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedY(theSprite, y, range) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithRect(theSprite, x, y, width, height) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
		
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Integer; cdecl; export;
	begin
		if SGSDK_Physics.HaveSpritesCollided(sprite1, sprite2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	{function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
		x, y: Integer; bounded: Boolean;
		vwPrtX, vwPrtY: Integer)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded, vwPrtX, vwPrtY) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function	HasSpriteCollidedWithBitmap1(theSprite: Sprite; theBitmap: Bitmap;
								x, y: Integer; bounded: Boolean)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function	HasSpriteCollidedWithBitmap2(theSprite: Sprite; theBitmap: Bitmap;
								x, y, vwPrtX, vwPrtY: Integer)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, vwPrtX, vwPrtY) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function	HasSpriteCollidedWithBitmap3(theSprite: Sprite; theBitmap: Bitmap;
								x, y: Integer)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2 : Bitmap;
								 x2, y2: Integer): Integer; cdecl; export;
	begin
		if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, image2, x2, y2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;}

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, bounded1, image2, x2, y2, bounded2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	{function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.CollisionWithinBitmapImages(image1, x1, y1, image2, x2, y2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;}

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		bounded1: Boolean; image2: Bitmap;
		x2, y2: Integer; bounded2: Boolean)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.CollisionWithinBitmapImages(image1, x1, y1, bounded1, image2, x2, y2, bounded2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	
	function	CreateVector(x,y : Single; invertY : boolean): Vector;  cdecl; export;
	begin
		result :=SGSDK_Physics.CreateVector(x, y, invertY);
	end;

	{function	CreateVector1(x,y : Single): Vector;  cdecl; export;
	begin
		result :=SGSDK_Physics.CreateVector(x, y);
	end;}

	function	AddVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.AddVectors(v1, v2);
	end;

	function	SubtractVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.SubtractVectors(v1, v2);
	end;

	function	InvertVector(v : Vector): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.InvertVector(v);
	end;

	function	ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.ChopVector(theVector, minX, maxX, minY, maxY);
	end;

	function	LimitVector(theVector: Vector; maxMagnitude: Single): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.LimitVector(theVector, maxMagnitude);
	end;

	function	GetUnitVector(theVector : Vector): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.GetUnitVector(theVector);
	end;

	function	IsZeroVector(theVector : Vector): Integer; cdecl; export;
	begin
		if SGSDK_Physics.IsZeroVector(theVector) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function	GetVectorMagnitude(theVector : Vector): Single; cdecl; export;
	begin
		result :=SGSDK_Physics.GetVectorMagnitude(theVector);
	end;
	
	function	DotProduct(v1, v2: Vector): Single; cdecl; export;
	begin
		result :=SGSDK_Physics.DotProduct(v1, v2);
	end;
	
	function 	MultiplyVector(v1: Vector; s1: Single): Vector; cdecl; export;
	begin
		result :=SGSDK_Physics.MultiplyVector(v1, s1);
	end;

 	function CalculateAngleNumber(x1, y1, x2, y2: Single): Single;  cdecl; export;
	begin
		result :=SGSDK_Physics.CalculateAngle(x1, y1, x2, y2);
	end;
	
	function CalculateAngleSprite(sprite1, sprite2: Sprite): Single;  cdecl; export;
	begin
		result :=SGSDK_Physics.CalculateAngle(sprite1, sprite2);
	end;

	function TranslationMatric(dx, dy: Single): Matrix2D; cdecl; export;
	begin
		result :=SGSDK_Physics.TranslationMatric(dx, dy);
	end;
	
	function ScaleMatrix(scale: Single): Matrix2D; cdecl; export;
	begin
		result :=SGSDK_Physics.ScaleMatrix(scale);
	end;
	
	function RotationMatrix(deg: Single): Matrix2D; cdecl; export;
	begin
		result :=SGSDK_Physics.RotationMatrix(deg);
	end;
	
	function Multiply(const m1, m2: Matrix2D): Matrix2D;  cdecl; export;
	begin
		result :=SGSDK_Physics.Multiply(m1, m2);
	end;
	
	function MultiplyVector(const m: Matrix2D; const v: Vector): Vector;  cdecl; export;
	begin
		result :=SGSDK_Physics.Multiply(m, v);
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Graphics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function GetSpriteBitmap(surface: Sprite; id: Integer): Bitmap; cdecl; export;
	begin
		result := surface.bitmaps[id];
	end;
	
	function GetSpriteX(surface: Sprite): Single; cdecl; export;
	begin
		result := surface.xPos;
	end;
	
	procedure SetSpriteX(surface: Sprite; val: Single); cdecl; export;
	begin
		surface.xPos := val;
	end;
	
	function GetSpriteY(surface: Sprite): Single; cdecl; export;
	begin
		result := surface.yPos;
	end;
	
	procedure SetSpriteY(surface: Sprite; val: Single); cdecl; export;
	begin
		surface.yPos := val;
	end;
	
	function GetSpriteCurrentFrame(surface: Sprite): Integer; cdecl; export;
	begin
		result := surface.currentFrame;
	end;
	
	procedure SetSpriteCurrentFrame(surface: Sprite; val: Integer); cdecl; export;
	begin
		surface.currentFrame := val;
	end;
	
	function GetSpriteUsePixelCollision(surface: Sprite): Integer; cdecl; export;
	begin
		if surface.usePixelCollision then
		begin
			result := -1
		end
		else
		begin
			result := 0
		end;
	end;
	
	procedure SetSpriteUsePixelCollision(surface: Sprite; val: Boolean); cdecl; export;
	begin
		surface.usePixelCollision := val;
	end;
	
	function NewSDLRect(x, y, w, h: Integer): SDL_Rect; cdecl; export;
	begin
		result := SGSDK_Graphics.NewSDLRect(x, y, w, h);
	end;
	
	function CreateBitmap(width, height: Integer): Bitmap; cdecl; export;
	begin
		result := SGSDK_Graphics.CreateBitmap(width, height);
	end;
	
	procedure OptimiseBitmap(surface: Bitmap); cdecl; export;
	begin
		SGSDK_Graphics.OptimiseBitmap(surface);
	end;
	
	function LoadBitmapWithTransparentColor(pathToBitmap: String; transparent: Boolean;
								transparentColor: Colour): Bitmap; cdecl; export;
	begin
		result := SGSDK_Graphics.LoadBitmap(pathToBitmap, transparent, transparentColor);
	end;
	
	{function LoadBitmap(pathToBitmap : String): Bitmap; cdecl; export;
	begin
		result := SGSDK_Graphics.LoadBitmap(pathToBitmap);
	end;}
	
	function LoadTransparentBitmap(pathToBitmap : String;
								transparentColor : Colour): Bitmap; cdecl; export;
	begin
		result := SGSDK_Graphics.LoadTransparentBitmap(pathToBitmap, transparentColor);
	end;
	
	procedure FreeBitmap(var bitmapToFree : Bitmap); cdecl; export;
	begin
		SGSDK_Graphics.FreeBitmap(bitmapToFree);
	end;
	
	function GetBitmapWidth(targetbitmap : Bitmap): Integer ; cdecl; export;
	begin
		result := targetbitmap.width;
	end;
	
	function GetBitmapHeight(targetbitmap : Bitmap): Integer; cdecl; export;
	begin
		result := targetbitmap.height;
	end;
	
	procedure ClearSurfaceWithColor(dest: Bitmap; toColour: Colour); cdecl; export;
	begin
		SGSDK_Graphics.ClearSurface(dest, toColour);
	end;
	
	{procedure ClearSurface(dest: Bitmap); cdecl; export;
	begin
		SGSDK_Graphics.ClearSurface(dest);
	end;}
	
	procedure DrawBitmapWithDestination(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
		cdecl; export;
	begin
		SGSDK_Graphics.DrawBitmap(dest, bitmapToDraw, x, y);
	end;
	
	procedure DrawBitmapPartWithDestination(dest: Bitmap; bitmapToDraw: Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawBitmapPart(dest, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
	end;
	
	procedure DrawPixelWithDestination(dest: Bitmap; theColour: Colour; x, y: Integer);
		cdecl; export;
	begin
		SGSDK_Graphics.DrawPixel(dest, theColour, x, y);
	end;
	
	procedure DrawRectangle4(dest: Bitmap; theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(dest, theColour, filled, xPos, yPos, width, height);
	end;
	
	procedure DrawRectangle3(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(dest, theColour, xPos, yPos, width, height);
	end;
	
	procedure FillRectangleWithDestination(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillRectangle(dest, theColour, xPos, yPos, width, height);
	end;

	procedure DrawLineWithDestination(dest: Bitmap; theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawLine(dest, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
	end;

	procedure DrawHorizontalLineWithDestination(dest: Bitmap; theColor: Color;
								 y, x1, x2: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawHorizontalLine(dest, theColor, y, x1, x2);
	end;

	procedure DrawVerticalLineWithDestination(dest: Bitmap; theColor: Color;
							 x, y1, y2: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawVerticalLine(dest, theColor, x, y1, y2);
	end;

	procedure DrawCircle4(dest: Bitmap; theColour: Colour; filled: Boolean;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(dest, theColour, filled, xc, yc, radius);
	end;

	procedure DrawCircle3(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(dest, theColour, xc, yc, radius);
	end;

	procedure FillCircle2(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillCircle(dest, theColour, xc, yc, radius);
	end;

	procedure DrawEllipse4(dest: Bitmap; theColour: Colour; filled: Boolean;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(dest, theColour, filled, xPos, yPos, width, height);
	end;

	procedure DrawEllipse3(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(dest, theColour, xPos, yPos, width, height);
	end;
	
	procedure FillEllipse2(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillEllipse(dest, theColour, xPos, yPos, width, height);
	end;
	
	procedure ClearScreen2(toColour : Colour); cdecl; export;
	begin
		SGSDK_Graphics.ClearScreen(toColour);
	end;

	procedure ClearScreen1(); cdecl; export;
	begin
		SGSDK_Graphics.ClearScreen();
	end;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawBitmap(bitmapToDraw, x, y);
	end;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawBitmapPart(bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
	end;
	
	procedure DrawPixel(theColour: Colour; x, y: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawPixel(theColour, x, y);
	end;

	procedure DrawRectangle2(theColour : Colour; filled : Boolean;
							xPos, yPos, width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(theColour, filled, xPos, yPos, width, height);
	end;

	procedure DrawRectangle1(theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(theColour, xPos, yPos, width, height);
	end;

	procedure FillRectangle1(theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillRectangle(theColour, xPos, yPos, width, height);
	end;

	procedure DrawLine1(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawLine(theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
	end;

	procedure DrawHorizontalLine1(theColor: Color; y, x1, x2: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawHorizontalLine(theColor, y, x1, x2);
	end;

	procedure DrawVerticalLine1(theColor: Color; x, y1, y2: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawVerticalLine(theColor, x, y1, y2);
	end;

	procedure DrawCircle2(theColour: Colour; filled: Boolean;
						 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(theColour, filled, xc, yc, radius);
	end;

	procedure DrawCircle1(theColour: Colour; xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(theColour, xc, yc, radius);
	end;

	procedure FillCircle1(theColour: Colour; xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillCircle(theColour, xc, yc, radius);
	end;

	procedure DrawEllipse2(theColour: Colour; filled: Boolean;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(theColour, filled, xPos, yPos, width, height);
	end;

	procedure DrawEllipse1(theColour: Colour;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(theColour, xPos, yPos, width, height);
	end;

	procedure FillEllipse1(theColour: Colour;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillEllipse(theColour, xPos, yPos, width, height);
	end;

	function CreateSprite(startBitmap : Bitmap): Sprite; cdecl; export;
	begin
		result := SGSDK_Graphics.CreateSprite(startBitmap);
	end;

	procedure FreeSprite(var spriteToFree : Sprite); cdecl; export;
	begin
		SGSDK_Graphics.FreeSprite(spriteToFree);
	end;

	function AddBitmapToSprite(spriteToAddTo : Sprite;
														 bitmapToAdd : Bitmap): Integer; cdecl; export;
	begin
		result := SGSDK_Graphics.AddBitmapToSprite(spriteToAddTo, bitmapToAdd);
	end;

	function CurrentHeight(sprite: Sprite): Integer; cdecl; export;
	begin
		result := SGSDK_Graphics.CurrentHeight(sprite);
	end;

	function CurrentWidth(sprite: Sprite): Integer; cdecl; export;
	begin
		result := SGSDK_Graphics.CurrentWidth(sprite);
	end;

	procedure DrawSprite1(spriteToDraw : Sprite); cdecl; export;
	begin
		SGSDK_Graphics.DrawSprite(spriteToDraw);
	end;

	procedure DrawSprite2(spriteToDraw : Sprite; vwPrtX, vwPrtY, vwPrtWidth,
											 vwPrtHeight : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawSprite(spriteToDraw, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight);
	end;

	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector); cdecl; export;
	begin
		SGSDK_Graphics.MoveSprite(spriteToMove, movementVector);
	end;

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer); cdecl; export;
	begin
		SGSDK_Graphics.MoveSpriteTo(SpriteToMove, x, y);
	end;

	function IsSpriteOffscreen1(theSprite : Sprite): Integer; cdecl; export;
	begin
		if SGSDK_Graphics.IsSpriteOffscreen(theSprite) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function IsSpriteOffscreen2(theSprite : Sprite; vwPrtX, vwPrtY,
															vwPrtWidth, vwPrtHeight : Integer) : Integer; cdecl; export;
	begin
		if SGSDK_Graphics.IsSpriteOffscreen(theSprite, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

exports

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	OpenGraphicsWindow,
	WindowCloseRequested,
	ProcessEvents,
	SetIcon,
	ChangeScreenSize,
	ToggleFullScreen,
	RefreshScreen,
	TakeScreenShot,
	ScreenWidth,
	ScreenHeight,
	ToSDLColor,
	GetColourBitmap,	
	GetColourRGBA,
	GetFramerate,	
	GetTicks,	
	Sleep,	
	GetPathToResourceWithKind,
	GetPathToResource,
	RegisterEventProcessor,	
	Cos,
	Sin,
	Tan,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	GetMousePosition,
	GetMouseMovement,
	IsMouseDown,
	IsMouseUp,
	MouseWasClicked,
	StartReadingText,
	IsReadingText,
	TextReadAsASCII,
	TextReadAsUNICODE,
	IsKeyPressed,
	WasKeyTyped,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	OpenAudio,
	CloseAudio,
	LoadSoundEffect,
	LoadMusic,
	FreeMusic,
	FreeSoundEffect,
	PlaySoundEffect,
	PlayMusic,
	PlaySoundEffectLoop,
	//PlayMusic1,
	IsMusicPlaying,
	IsSoundEffectPlaying,
	StopSoundEffect,
	StopMusic,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	LoadFont,
	SetFontStyle,
	FreeFont,
	DrawText,
	DrawTextLines,
	DrawText1,
	DrawTextLines1,
	TextWidth,
	TextHeight,
	DrawFramerate,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	HasSpriteCollidedX,
	HasSpriteCollidedY,
	HasSpriteCollidedWithRect,
	HaveSpritesCollided,
	//HasSpriteCollidedWithBitmap,
	//HasSpriteCollidedWithBitmap1,
	//HasSpriteCollidedWithBitmap2,
	//HasSpriteCollidedWithBitmap3,
	HaveBitmapsCollided,
	//HaveBitmapsCollided1,
	CollisionWithinBitmapImages,
	//CollisionWithinBitmapImages1,
	CreateVector,
	//CreateVector1,
	AddVectors,
	SubtractVectors,
	InvertVector,
	ChopVector,
	LimitVector,
	GetUnitVector,
	IsZeroVector,
	GetVectorMagnitude,
	DotProduct,
	MultiplyVector,
	CalculateAngleNumber,
	CalculateAngleSprite,
	TranslationMatric,
	ScaleMatrix,
	RotationMatrix,
	Multiply,
	MultiplyVector,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Graphics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	GetSpriteBitmap,
	GetSpriteX,
	GetSpriteY,
	GetSpriteCurrentFrame,
	GetSpriteUsePixelCollision,
	SetSpriteX,
	SetSpriteY,
	SetSpriteCurrentFrame,
	SetSpriteUsePixelCollision,
	NewSDLRect,
	CreateBitmap,
	OptimiseBitmap,
	LoadBitmapWithTransparentColor,
	//LoadBitmap,
	LoadTransparentBitmap,
	FreeBitmap,
	GetBitmapWidth,
	GetBitmapHeight,
	ClearSurfaceWithColor,
	//ClearSurface,
	DrawBitmapWithDestination,
	DrawBitmapPartWithDestination,
	DrawPixelWithDestination,
	DrawRectangleWithDestination,
	//DrawRectangle3,
	FillRectangleWithDestination,
	DrawLineWithDestination,
	DrawHorizontalLineWithDestination,
	DrawVerticalLineWithDestination,
	DrawCircle4,
	DrawCircle3,
	FillCircle2,
	DrawEllipse4,
	DrawEllipse3,
	FillEllipse2,
	ClearScreen2,
	ClearScreen1,
	DrawBitmap,
	DrawBitmapPart,
	DrawPixel,
	DrawRectangle2,
	DrawRectangle1,
	FillRectangle1,
	DrawLine1,
	DrawHorizontalLine1,
	DrawVerticalLine1,
	DrawCircle2,
	DrawCircle1,
	FillCircle1,
	DrawEllipse2,
	DrawEllipse1,
	FillEllipse1,
	CreateSprite,
	FreeSprite,
	AddBitmapToSprite,
	CurrentHeight,
	CurrentWidth,
	DrawSprite1,
	DrawSprite2,
	MoveSprite,
	MoveSpriteTo,
	IsSpriteOffscreen1,
	IsSpriteOffscreen2
	;
end.