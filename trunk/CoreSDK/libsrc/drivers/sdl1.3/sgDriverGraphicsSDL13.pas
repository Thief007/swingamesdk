unit sgDriverGraphicsSDL13;
//=============================================================================
// sgDriverGraphicsSDL.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
uses sgTypes, SDL13;
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
	
	procedure LoadSDL13GraphicsDriver();
	function ToGfxColorProcedure(val : Color): Color;
	procedure DrawSurfaceToRenderer();
	procedure DrawDirtyScreen();
	
implementation
	uses sgDriverGraphics, sysUtils, sgShared, sgGeometry, 
		SDL13_gfx, SDL13_Image, sgSavePNG, sgInputBackend, sgDriverSDL13, sgDriverImages, sgUtils;	
	{
	procedure DisposeScreen(scrn : Pointer);
	begin
	  if Assigned(scrn) then
	  begin
	    if Assigned(PSDL13Screen(_screen)^.window) then
	      Dispose(PSDL13Screen(_screen)^.window);
  	  if Assigned(PSDL13Screen(_screen)^.renderer) then
  	    Dispose(PSDL13Screen(_screen)^.renderer);
    	Dispose(PSDL13Screen(_screen));
	  end;
	end;
}


  procedure DrawDirtyScreen();
  begin  
	  if _ScreenDirty then
    begin
      DrawSurfaceToRenderer();
    end;
  end;

  procedure DisposeSurface(surface : Pointer);
  begin
    if Assigned(surface) then
    begin
      if Assigned(PSDL13Surface(surface)^.surface) then
        SDL_FreeSurface(PSDL_Surface(PSDL13Surface(surface)^.surface));
  	  if Assigned(PSDL13Surface(surface)^.texture) then
  	    SDL_DestroyTexture(PSDL_Texture(PSDL13Surface(surface)^.texture));
    	Dispose(PSDL13Surface(surface));
    end;
  end;
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color; 
  begin
    result := SDL_MapRGBA(PSDL_Surface(PSDL13Surface(screen^.surface)^.surface)^.format, red, green, blue, alpha);
  end;
  
  
  // Used to draw Primitive Shapes to a Surface -> Bitmap - > Renderer.
  procedure DrawSurfaceToRenderer();
  var
    surf : PSDL_Surface;
    offset : Rectangle;
  begin  
    _ScreenDirty := False;
    offset := RectangleFrom(0, 0, screen^.width, screen^.height); 
    SDL_SetSurfaceAlphaMod(GetSurface(screen), 255);
    ImagesDriver.BlitSurface(screen, nil, nil, @offset);
    SDL_FillRect(GetSurface(screen), nil, RGBAColorProcedure(0, 0, 0, 0));   
  end;
	
	function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
	var
    pixel, pixels: PUint32;
    offset: Longword;
  {$IFDEF FPC}
    pixelAddress: PUint32;
  {$ELSE}
    pixelAddress: Longword;
  {$ENDIF}
    surface : PSDL_Surface;
	begin
		if not Assigned(bmp) then begin RaiseWarning('SDL1.3 Graphics Driver - GetPixel32Procedure recieved empty Bitmap'); exit; end;
		
	  if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
    begin
      result := 0;
      exit;
    end;  
		 
		surface := GetSurface(bmp);
		
		
    //Convert the pixels to 32 bit
    pixels := surface^.pixels;

    //Get the requested pixel
    offset := (( y * surface^.w ) + x) * surface^.format^.BytesPerPixel;

    {$IFDEF FPC}
      pixelAddress := pixels + (offset div 4);
      pixel := PUint32(pixelAddress);
    {$ELSE}
      pixelAddress := Longword(pixels) + offset;
      pixel := Ptr(pixelAddress);
    {$ENDIF}

    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
    case surface^.format^.BytesPerPixel of
      1: result := pixel^ and $000000ff;
      2: result := pixel^ and $0000ffff;
      3: result := pixel^ and $00ffffff;
      4: result := pixel^;
    else
      RaiseException('Unsuported bit format...');
      exit;
    end;
    {$ELSE}
    case surface^.format^.BytesPerPixel of
      1: result := pixel^ and $ff000000;
      2: result := pixel^ and $ffff0000;
      3: result := pixel^ and $ffffff00;
      4: result := pixel^;
    else
      raise Exception.Create('Unsuported bit format...')
    end;
    {$IFEND}
	end;
		
	procedure PutPixelProcedure(bmp: Bitmap; clr: Color; x, y: Longint);
  var
      p:    ^Color;
      bpp:  Longint;
  begin
      if not assigned(bmp) then begin RaiseWarning('SDL1.3 Graphics Driver - PutPixelProcedure recieved empty Bitmap'); exit; end;
        
      bpp := PSDL_Surface(bmp^.surface)^.format^.BytesPerPixel;
      // Here p is the address to the pixel we want to set
      p := PSDL_Surface(bmp^.surface)^.pixels + y * PSDL_Surface(bmp^.surface)^.pitch + x * bpp;

      if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
      p^ := clr;
  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin  
    SDL_GetRGBA(c, GetSurface(screen)^.format, @r, @g, @b, @a);
  end;
  
  function ToGfxColorProcedure(val : Color): Color; 
  var
    r, g, b, a: Byte;
  begin
    ColorComponentsProcedure(val, r, g, b, a);
    result := (r shl 24) or (g shl 16) or (b shl 8) or a;
  end;
  
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  begin
    if w < 0 then
    begin
      x += w;
      w := -w;
    end;
    if h < 0 then
    begin
      y += h;
      h := -h;
    end;
    
    result.x := x;
    result.y := y;
    result.w := Word(w);
    result.h := Word(h);
  end;  
  
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  begin
      result := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
	var
    r, g, b, a : Uint8;
	begin
	  if dest <> screen then
	  begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - DrawTriangleProcedure recieved empty Bitmap'); exit; end;
      trigonColor(GetSurface(dest), Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), clr);
	    exit;
	  end;
	  ColorComponentsProcedure(clr, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
	  SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x1), Round(y1), Round(x2), Round(y2));
	  SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x2), Round(y2), Round(x3), Round(y3));
	  SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x3), Round(y3), Round(x1), Round(y1));
  end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    if dest <> screen then
	  begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillTriangleProcedure recieved empty Bitmap'); exit; end;
      filledTrigonColor(GetSurface(dest), Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), clr);
      exit;
    end;  
    filledTrigonColor(GetSurface(screen), Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColorProcedure(clr));
    _ScreenDirty := True;
  end;
	
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if dest <> screen then
	  begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
      circleColor(GetSurface(dest), Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
    end;
     circleColor(GetSurface(screen), Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
    _ScreenDirty := True;
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if dest = screen then
      _ScreenDirty := True;
	  filledCircleColor(GetSurface(dest), Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
  end;
  
	// This procedure draws an Ellipse to a bitmap
	procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin	  
    if dest <> screen then
	  begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
      filledEllipseColor(GetSurface(dest), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
      exit;
    end;
    filledEllipseColor(GetSurface(screen), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
    _ScreenDirty := True;
	end;
	
	// This procedure draws an Ellipse to a bitmap
	procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
    if dest <> screen then
	  begin  
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
      ellipseColor(GetSurface(dest), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
      exit;
    end;
    ellipseColor(GetSurface(screen), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
    _ScreenDirty := True;
	end;
	
	// This procedure draws a filled rectangle to a bitmap
	procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	var
	  sdlrect : SDL_Rect;
    r, g, b, a : Uint8;
	begin	
	  if clr = $00000000 then
	    exit;
	  if dest <> screen then
	  begin
    	if not Assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillRectangleProcedure recieved empty Bitmap'); exit; end;
	    boxColor(GetSurface(dest),  RoundInt(rect.x), RoundInt(rect.y),  RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), ToGfxColorProcedure(clr));	  
      exit;
    end;
	  sdlrect := NewSDLRect(rect);
	  ColorComponentsProcedure(clr, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
	  SDL_RenderFillRect(PSDL13Screen(_screen)^.renderer, @sdlrect);
	end;
	
	procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	var
	  sdlrect : SDL_Rect;
    r, g, b, a : Uint8;
	begin
	  if dest <> screen then
	  begin    
  		if not Assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - DrawRectangleProcedure recieved empty Bitmap'); exit; end;
      rectangleColor(GetSurface(dest),  RoundInt(rect.x), RoundInt(rect.y),  RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), ToGfxColorProcedure(clr));	  
	    exit;
	  end;
	  sdlrect := NewSDLRect(rect);
	  ColorComponentsProcedure(clr, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
	  SDL_RenderDrawRect(PSDL13Screen(_screen)^.renderer, @sdlrect);
	end;
	
	// This procedure draws a line on a bitmap between two points
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
	var
    r, g, b, a : Uint8;
	begin
	  if dest <> screen then
	  begin
  		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.3 Graphics Driver - DrawLineProcedure recieved empty Bitmap'); exit; end;
  		// Add check to ensure points are less than 32,767
  		aalineColor(dest^.surface, x1, y1, x2, y2, ToGfxColorProcedure(clr));
	    exit;
	  end;
	  ColorComponentsProcedure(clr, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
	  SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, x1, y1, x2, y2);
	end;
	
	// This procedure sets the color of a pixel on a bitmap
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	var
    r, g, b, a : Uint8;
	begin
	  ColorComponentsProcedure(clr, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
	  SDL_RenderDrawPoint(PSDL13Screen(_screen)^.renderer, x, y);
	//	if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.3 Graphics Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
	//	pixelColor(dest^.surface, x, y, ToGfxColorProcedure(clr));
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  var
    SDLrect: SDL_Rect;
  begin
    if dest = nil then begin RaiseWarning('SDL1.3 Graphics Driver - SetClipRectangle recieved empty Bitmap'); exit; end;
    
    SDLrect := NewSDLRect(Round(rect.x), Round(rect.y), rect.width, rect.height);
    SDL_SetClipRect(dest^.surface, @SDLrect);
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    if bmp = nil then begin RaiseWarning('SDL1.3 Graphics Driver - ResetClip recieved empty Bitmap'); exit; end;
    
    SetLength(bmp^.clipStack, 0);
    SDL_SetClipRect(bmp^.surface, nil);
  end;

  procedure SetVideoModeFullScreenProcedure();
  var
    fullScreen : SDL_Bool = SDL_True;
    flags : Uint32;
    //noFrame : boolean;
  begin    
    flags := SDL_GetWindowFlags(PSDL13Screen(_screen)^.window);

    if (flags or Uint32(SDL_WINDOW_FULLSCREEN)) = Uint32(SDL_WINDOW_FULLSCREEN) then
      fullScreen := SDL_False;
      
    SDL_SetWindowFullScreen(PSDL13Screen(_screen)^.window, fullScreen);
  end;
  
  procedure SetVideoModeNoFrameProcedure();
  begin    
    // TODO: need to recreate window to switch to borderless... check further if needed
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen(screenWidth, screenHeight: Longint);
  var
    bpp        : integer;
    rMask, gMask, bMask, aMask, format : Uint32; 
  begin
    if screen = nil 
      then New(screen)
    else if (screen^.surface <> nil) then 
      DisposeSurface(screen^.surface);
	  
	  format := SDL_GetWindowPixelFormat(PSDL13Screen(_screen)^.window);
	  
	  SDL_PixelFormatEnumToMasks(format, @bpp, @rMask, @gMask, @bMask, @aMask);
	  
	  New(PSDL13Surface(screen^.surface));
    
    PSDL13Surface(screen^.surface)^.surface :=  SDL_CreateRGBSurface(SDL_HWSURFACE, screenWidth, screenHeight, 32, 
                                                                    rMask, gMask, bMask, 
                                                                    $ffffffff and not RMask and not GMask and not BMask);                                                         
    screen^.width := screenWidth;
    screen^.height := screenHeight;
    
    screenRect := RectangleFrom(0,0, screenWidth, screenHeight);
    
    if (Assigned(PSDL13Screen(_screen)^.renderer)) then
      SDL_DestroyRenderer(PSDL13Screen(_screen)^.renderer);
    
    // We must call SDL_CreateRenderer in order for draw calls to affect this window.
    PSDL13Screen(_screen)^.renderer := SDL_CreateRenderer(PSDL13Screen(_screen)^.window, -1, LongWord(SDL_RENDERER_ACCELERATED) {or LongWord(SDL_RENDERER_PRESENTVSYNC)});
    
    // Select the color for drawing. It is set to red here.
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, 0, 0, 0, 255);

    // Clear the entire screen to our selected color.
    SDL_RenderClear(PSDL13Screen(_screen)^.renderer);
    // Up until now everything was drawn behind the scenes.
    // This will show the new, red contents of the window.
    SDL_RenderPresent(PSDL13Screen(_screen)^.renderer);
  end;
  
  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  var
      sdlScreen : PSDL13Screen;
  begin
    
    // Initialize SDL.
    if (SDL_Init(SDL_INIT_VIDEO) < 0) then exit;
    
    _screen := New(PSDL13Screen);
    
    // Create the window where we will draw.
    PSDL13Screen(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, screenWidth, screenHeight, Uint32(SDL_WINDOW_SHOWN));
{
    // We must call SDL_CreateRenderer in order for draw calls to affect this window.
    PSDL13Screen(_screen)^.renderer := SDL_CreateRenderer(PSDL13Screen(_screen)^.window, -1, LongWord(SDL_RENDERER_ACCELERATED) or LongWord(SDL_RENDERER_PRESENTVSYNC));
    
    // Select the color for drawing. It is set to red here.
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, 0, 0, 0, 255);

    // Clear the entire screen to our selected color.
    SDL_RenderClear(PSDL13Screen(_screen)^.renderer);
    // Up until now everything was drawn behind the scenes.
    // This will show the new, red contents of the window.
    SDL_RenderPresent(PSDL13Screen(_screen)^.renderer); }

    _SetupScreen(screenWidth, screenHeight);
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin      
  	if not Assigned(GetSurface(screen)) then begin RaiseWarning('SDL1.3 Graphics Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
  //	  _SetupScreen(width, height);
 //   stringColor(GetSurface(screen), width div 2 - 30, height div 2, PChar(msg), ToGfxColorProcedure(strColor));
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    WriteLn('Resizing Screen');
    SDL_SetWindowSize(PSDL_Window(PSDL13Screen(_screen)^.window), newWidth, newHeight);
    _SetupScreen(newWidth, newHeight);
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
		if not Assigned(GetSurface(bmpToSave)) then begin RaiseWarning('SDL1.3 Graphics Driver - SaveImageProcedure recieved empty Bitmap'); exit; end;
    result := png_save_surface(path, GetSurface(bmpToSave));
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin  
    DrawCollectedText(screen);
    if _ScreenDirty then
    begin
      DrawSurfaceToRenderer();
    end;
    SDL_RenderPresent(PSDL13Screen(_screen)^.renderer);
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
		if not Assigned(bmp^.surface) then begin RaiseWarning('SDL1.3 Graphics Driver - ColorFromProcedure recieved empty Bitmap'); exit; end;
    result := SDL_MapRGBA(GetSurface(screen)^.format, r, g, b, a);
  end;

  function GetSurfaceWidthProcedure(src : Bitmap) : LongInt;
  begin
    result := GetSurface(src)^.w;
  end;
  
  function GetSurfaceHeightProcedure(src : Bitmap) : LongInt;
  begin
    result := GetSurface(src)^.h;
  end;
  
  function SurfaceFormatAssignedProcedure(src: Bitmap) : Boolean;
  begin
    result := Assigned(GetSurface(src)^.format );
  end; 
  
  procedure GetRGBProcedure(pixel : Byte; r,g,b : Byte); 
  var
    fmt : PSDL_Surface;
  begin
    fmt := GetSurface(screen);
    SDL_GetRGB(pixel,fmt^.format, @r,@g,@b);    
  end;

  function GetScreenWidthProcedure(): LongInt; 
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(PSDL13Screen(_screen)^.window,@w, @h);
    result := w;
  end;
  
  function GetScreenHeightProcedure(): LongInt;
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(PSDL13Screen(_screen)^.window,@w, @h);
    result := h;
  end;
  

	procedure LoadSDL13GraphicsDriver();
	begin
		GraphicsDriver.GetPixel32               := @GetPixel32Procedure;
		GraphicsDriver.PutPixel                 := @PutPixelProcedure;		
		GraphicsDriver.FillTriangle             := @FillTriangleProcedure;
		GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;		
		GraphicsDriver.FillCircle               := @FillCircleProcedure;
		GraphicsDriver.DrawCircle               := @DrawCircleProcedure;		
		GraphicsDriver.FillEllipse              := @FillEllipseProcedure;
		GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;		
		GraphicsDriver.FillRectangle            := @FillRectangleProcedure;
		GraphicsDriver.DrawLine                 := @DrawLineProcedure;
		GraphicsDriver.SetPixelColor            := @SetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @ResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;    
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @SaveImageProcedure;
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;
    GraphicsDriver.GetSurfaceWidth          := @GetSurfaceWidthProcedure;
    GraphicsDriver.GetSurfaceHeight         := @GetSurfaceHeightProcedure;
    GraphicsDriver.GetRGB                   := @GetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;
	end;
end.