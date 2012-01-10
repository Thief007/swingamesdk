unit sgDriverGraphicsSDL;
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


	
	procedure LoadSDLGraphicsDriver();
		
implementation
	uses sgDriverGraphics, sysUtils,sgTypes, sgShared, sgGeometry,
		SDL_gfx, SDL, SDL_Image, sgSavePNG, sgInputBackend;
	
	function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
	begin
		if not Assigned(bmp) then begin RaiseWarning('SDL1.2 Driver - GetPixel32Procedure recieved empty Bitmap'); exit; end;
		
	  if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
    begin
      result := 0;
      exit;
    end;  
		  
		result := GetPixel32(bmp^.surface, x, y);
	end;
		
	procedure PutPixelProcedure(bmp: Bitmap; clr: Color; x, y: Longint);
  var
      p:    ^Color;
      bpp:  Longint;
  begin
      if not assigned(bmp) then begin RaiseWarning('SDL1.2 Driver - PutPixelProcedure recieved empty Bitmap'); exit; end;
        
      bpp := bmp^.surface^.format^.BytesPerPixel;
      // Here p is the address to the pixel we want to set
      p := bmp^.surface^.pixels + y * bmp^.surface^.pitch + x * bpp;

      if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
      p^ := clr;
  end;
  
  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawTriangleProcedure recieved empty Bitmap'); exit; end;
    trigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), clr);
  end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillTriangleProcedure recieved empty Bitmap'); exit; end;
    filledTrigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), clr);
  end;
	
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawCircleProcedure recieved empty Bitmap'); exit; end;
    aacircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColor(clr));
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
    filledCircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColor(clr));
  end;
  
	// This procedure draws an Ellipse to a bitmap
	procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillEllipseProcedure recieved empty Bitmap'); exit; end;
		filledEllipseColor(dest^.surface, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, ToGfxColor(clr));
	end;
	
	// This procedure draws an Ellipse to a bitmap
	procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawEllipseProcedure recieved empty Bitmap'); exit; end;
		aaellipseColor(dest^.surface, xPos, yPos, halfWidth, halfHeight, ToGfxColor(clr));
	end;
	
	// This procedure draws a filled rectangle to a bitmap
	procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillRectangleProcedure recieved empty Bitmap'); exit; end;
		boxColor(dest^.surface, 
			RoundInt(rect.x), RoundInt(rect.y), 
			RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), 
			ToGfxColor(clr));
	end;
	
	procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawRectangleProcedure recieved empty Bitmap'); exit; end;
    rectangleColor(dest^.surface, 
      RoundInt(rect.x), RoundInt(rect.y), 
      RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), 
      ToGfxColor(clr));
	end;
	
	// This procedure draws a line on a bitmap between two points
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawLineProcedure recieved empty Bitmap'); exit; end;
		// Add check to ensure points are less than 32,767
		aalineColor(dest^.surface, x1, y1, x2, y2, ToGfxColor(clr));
	end;
	
	// This procedure sets the color of a pixel on a bitmap
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
		pixelColor(dest^.surface, x, y, ToGfxColor(clr));
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  var
    SDLrect: SDL_Rect;
  begin
    if dest = nil then begin RaiseWarning('SDL1.2 Driver - SetClipRectangle recieved empty Bitmap'); exit; end;
    
    SDLrect := NewSDLRect(Round(rect.x), Round(rect.y), rect.width, rect.height);
    SDL_SetClipRect(dest^.surface, @SDLrect);
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    if bmp = nil then begin RaiseWarning('SDL1.2 Driver - ResetClip recieved empty Bitmap'); exit; end;
    
    SetLength(bmp^.clipStack, 0);
    SDL_SetClipRect(bmp^.surface, nil);
  end;

  procedure SetVideoModeFullScreenProcedure();
  var
    oldScr: PSDL_Surface;
  begin    
    oldScr := _screen;
    _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_FULLSCREEN);
  end;
  
  procedure SetVideoModeNoFrameProcedure();
  var
    oldScr: PSDL_Surface;
  begin    
    oldScr := _screen;
    _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_NOFRAME);
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen();
  begin
    if screen = nil then New(screen)
    else if (screen^.surface <> nil) then SDL_FreeSurface(screen^.surface);
	
    with _screen^.format^ do
    begin
      screen^.surface := SDL_CreateRGBSurface(SDL_HWSURFACE,
                                             _screen^.w, _screen^.h, 32,
                                             RMask, GMask, BMask, 
                                             $ffffffff and not RMask
                                                and not GMask
                                                and not BMask);
      
      //WriteLn(RMask, ':', GMask, ':', BMask, ':', screen^.surface^.format^.AMask);
      
      //Turn off alpha blending for when scr is blit onto _screen
      SDL_SetAlpha(screen^.surface, 0, 255);
      SDL_FillRect(screen^.surface, @screen^.surface^.clip_rect, 0);

      baseSurface := screen^.surface;

      screen^.width := _screen^.w;
      screen^.height := _screen^.h;
      screenRect := RectangleFrom(0,0, screen^.width, screen^.height);
    end;
  end;
  
  /// Sets up the graphical window for the specified width and height.
  /// Sets the caption of the window, and the icon if one is specified.
  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  var
    icon: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'InitSDL');
    {$ENDIF}

    if (screenWidth < 1) or (screenHeight < 1) then
    begin
      RaiseWarning('Screen Width and Height must be greater then 0 when opening a Graphical Window');
      exit;
    end;

    if Length(iconFile) > 0 then
    begin
      try
        icon := IMG_Load(PChar(iconFile));
        SDL_WM_SetIcon(icon, 0);
        SDL_FreeSurface(icon);
      except
        RaiseWarning('The icon file specified could not be loaded');
        exit;
      end;
    end;

    _screen := SDL_SetVideoMode(screenWidth, screenHeight, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if _screen = nil then
    begin
      RaiseWarning('Unable to create window drawing surface... ' + SDL_GetError());
      exit;
    end;

    _SetupScreen();
    if length(caption) > 0 then
      SDL_WM_SetCaption(PChar(caption), nil);

    {$IFDEF TRACE}
      TraceExit('sgGraphics', '_InitSDL');
    {$ENDIF}
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin      
  	if not Assigned(screen^.surface) then begin RaiseWarning('SDL1.2 Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
    SDL_FillRect(screen^.surface, nil, bgColor);
    stringColor(screen^.surface, width div 2 - 30, height div 2, PChar(msg), ToGFXColor(strColor));
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  var
    oldScr: PSDL_Surface;
  begin
    oldScr := _screen;
    _screen := SDL_SetVideoMode(newWidth, newHeight, 32, oldScr^.flags);
    _SetupScreen();
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
		if not Assigned(bmpToSave^.surface) then begin RaiseWarning('SDL1.2 Driver - SaveImageProcedure recieved empty Bitmap'); exit; end;
    result := png_save_surface(path, bmpToSave^.surface);
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin  
  	if not Assigned(screen^.surface) then begin RaiseWarning('SDL1.2 Driver - RefreshScreenProcedure recieved empty Bitmap'); exit; end;
    DrawCollectedText(screen);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);
  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin
    SDL_GetRGBA(c, baseSurface^.Format, @r, @g, @b, @a);
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
		if not Assigned(bmp^.surface) then begin RaiseWarning('SDL1.2 Driver - ColorFromProcedure recieved empty Bitmap'); exit; end;
    result := SDL_MapRGBA(bmp^.surface^.format, r, g, b, a);
  end;
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color;
  begin
    result := SDL_MapRGBA(baseSurface^.format, red, green, blue, alpha);
  end;
  
	procedure LoadSDLGraphicsDriver();
	begin
		Write('Loading SDL 1.2 Graphics Driver...');
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
		WriteLn('Finished.');
	end;
end.