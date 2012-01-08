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
	uses sgTypes, sgShared, sgGeometry,
		SDL_gfx, SDL, SDL_Image, sgSavePNG;
	
	procedure LoadSDLGraphicsDriver();
		
implementation
	uses sgDriverGraphics, sysUtils;
	
	// This procedure draws a filled rectangle to a bitmap
	procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then RaiseWarning('SDL1.2 Driver - FillRectangleProcedure recieved empty Bitmap');
		boxColor(dest^.surface, 
			RoundInt(rect.x), RoundInt(rect.y), 
			RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), 
			ToGfxColor(clr));
	end;
	
	procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then RaiseWarning('SDL1.2 Driver - DrawRectangleProcedure recieved empty Bitmap');
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
      RaiseException('Screen Width and Height must be greater then 0 when opening a Graphical Window');
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
      RaiseException('Unable to create window drawing surface... ' + SDL_GetError());
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
    result := png_save_surface(path, bmpToSave^.surface);
  end;
  
	procedure LoadSDLGraphicsDriver();
	begin
		Write('Loading SDL 1.2 Graphics Driver...');
		GraphicsDriver.FillRectangle := @FillRectangleProcedure;
		GraphicsDriver.DrawLine := @DrawLineProcedure;
		GraphicsDriver.SetPixelColor := @SetPixelColorProcedure;
    GraphicsDriver.DrawRectangle := @DrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle := @SetClipRectangleProcedure;
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;
    GraphicsDriver.ResizeGraphicsWindow := @ResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage := @SaveImageProcedure;
		WriteLn('Finished.');
	end;
end.