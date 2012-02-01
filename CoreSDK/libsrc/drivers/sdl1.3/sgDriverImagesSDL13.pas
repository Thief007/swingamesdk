unit sgDriverImagesSDL13;
//=============================================================================
// sgDriverImagesSDL13.pas
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
	
	procedure LoadSDL13ImagesDriver();
		
implementation
	uses sgDriverImages, sgShared, sgTypes, SysUtils, sgGraphics, sgDriver, sgSharedUtils,
	     SDL13_gfx, SDL13, SDL13_Image, sgDriverGraphics, sgDriverGraphicsSDL13, sgDriverSDL13; // sdl;
		
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
 	begin	  
 	  CheckAssigned('SDL1.3 ImagesDriver - InitBitmapColorsProcedure recieved unassigned Bitmap', bmp);
   	CheckAssigned('SDL1.3 ImagesDriver - InitBitmapColorsProcedure recieved empty Bitmap Surface', bmp^.surface);
    SDL_SetAlpha(GetSurface(bmp), SDL_SRCALPHA, 0);
    SDL_FillRect(GetSurface(bmp), nil, ColorTransparent);
 	end;
     	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  if not assigned(bmp) then 
	    result := false
	  else if not Assigned(bmp^.surface) then
	    result := False
	  else
	    result := Assigned(GetSurface(bmp));
	end;
  

	procedure CreateBitmapProcedure(bmp : Bitmap; width, height : LongInt);
	begin
	  bmp^.surface := New(PSDL13Surface);
 	  CheckAssigned('SDL1.3 ImagesDriver - CreateBitmapProcedure recieved unassigned Bitmap', bmp);
		if (GetSurface(screen) = nil) or (GetSurface(screen)^.format = nil) then
    begin
      RaiseWarning('Creating ARGB surface as screen format unknown.');
      PSDL13Surface(bmp^.surface)^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, $00FF0000, $0000FF00, $000000FF, $FF000000);
    end
    else
    begin
      with GetSurface(screen)^.format^ do
      begin
        PSDL13Surface(bmp^.surface)^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, RMask, GMask, BMask, AMask);
      end;
    end;
    PSDL13Surface(bmp^.surface)^.texture := nil;
	end;

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; transparentColor: Color);
  var
    r, c: Longint;
  begin
 	  CheckAssigned('SDL1.3 ImagesDriver - SetNonTransparentPixels recieved unassigned Bitmap', bmp);

    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := (GraphicsDriver.GetPixel32(bmp, c, r) <> transparentColor);
      end;
    end;
  end;
  
  procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
  var
    r, c: Longint;
    hasAlpha: Boolean;
  begin
    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);
    hasAlpha := PSDL13Surface(bmp^.surface)^.surface^.format^.BytesPerPixel = 4;

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := (not hasAlpha) or ((GraphicsDriver.GetPixel32(bmp, c, r) and SDL_Swap32($000000FF)) > 0);
      end;
    end;
  end;
  
  function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  var
    loadedImage: PSDL_Surface;
    surf : PSDL_Surface;
    offset : Rectangle;
  begin
    result := nil; //start at nil to exit cleanly on error
    
    //Load the image
    loadedImage := IMG_Load(PChar(filename));

 	  CheckAssigned('SDL1.3 ImagesDriver - Error loading image: ' + filename + ': ' + SDL_GetError(), loadedImage);

    // Image loaded, so create SwinGame bitmap
    new(result);
    result^.surface := New(PSDL13Surface);

    result^.width     := loadedImage^.w;
    result^.height    := loadedImage^.h;    
    PSDL13Surface(result^.surface)^.surface := loadedImage;
    
    //Determine pixel level collision data
    if transparent then
    begin
      offset.x := 0;
      offset.y := 0;
      offset.width := result^.width;
      offset.height := result^.height;
      
      SetNonTransparentPixels(result, transparentColor);
      PSDL13Surface(result^.surface)^.surface := SDL_CreateRGBSurface(SDL_SWSURFACE, result^.width, result^.height, 32, 0, 0, 0, 0);
      WriteLn('Color');
      SDL_SetColorKey(GetSurface(result), SDL_SRCCOLORKEY, transparentColor);
      WriteLn('Key Done');
      SDL_UpperBlit(loadedImage, @offset, GetSurface(result), @offset);
      PSDL13Surface(result^.surface)^.texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, GetSurface(result));
  	  SDL_RenderCopy(PSDL13Screen(_screen)^.renderer, PSDL13Surface(result^.surface)^.texture, @offset, @offset);
  	  SDL_FreeSurface(PSDL13Surface(result^.surface)^.surface);
    end else begin    
      SetNonAlphaPixelsProcedure(result);
      PSDL13Surface(result^.surface)^.texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, loadedImage);
    end;
    // Free the loaded image; if its not the result's surface
    PSDL13Surface(result^.surface)^.surface := nil;
    SDL_FreeSurface(loadedImage);
	end; 
	
	procedure FreeSurfaceProcedure(bmp : Bitmap);
	begin
	  //Free the surface
    if Assigned(GetSurface(bmp)) then
    begin
      SDL_FreeSurface(GetSurface(bmp));
    end;
    if Assigned(PSDL13Surface(bmp^.surface)^.texture) then
    begin
      SDL_DestroyTexture(PSDL13Surface(bmp^.surface)^.texture);
    end;
    
    PSDL13Surface(bmp^.surface)^.surface := nil;
    PSDL13Surface(bmp^.surface)^.texture := nil;
    bmp^.surface := nil;    
	end;	
	
	procedure MakeOpaqueProcedure(bmp : Bitmap);
	begin
 	  CheckAssigned('SDL1.3 ImagesDriver - MakeOpaqueProcedure recieved unassigned Bitmap', bmp);
   	CheckAssigned('SDL1.3 ImagesDriver - MakeOpaqueProcedure recieved empty Bitmap Surface', bmp^.surface);
    SDL_SetAlpha(GetSurface(bmp), 0, 255);
	end;

	procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
	begin
   	CheckAssigned('SDL1.3 ImagesDriver - SetOpacityProcedure received unassigned Bitmap', bmp);
    CheckAssigned('SDL1.3 ImagesDriver - SetOpacityProcedure recieved empty Bitmap Surface', bmp^.surface);
    SDL_SetAlpha(GetSurface(bmp), SDL_SRCALPHA, RoundUByte(pct * 255));
	end;

	procedure MakeTransparentProcedure(bmp : Bitmap);
	begin
   	CheckAssigned('SDL1.3 ImagesDriver - MakeTransparentProcedure recieved empty Bitmap', bmp);
    CheckAssigned('SDL1.3 ImagesDriver - MakeTransparentProcedure recieved empty Bitmap Surface', bmp^.surface);
    
    SDL_SetAlpha(GetSurface(bmp), SDL_SRCALPHA, 0);
	end;

	procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
	begin
   	CheckAssigned('SDL1.3 ImagesDriver - RotateScaleSurfaceProcedure recieved unassigned Result Bitmap', resultBmp);
   	CheckAssigned('SDL1.3 ImagesDriver - RotateScaleSurfaceProcedure recieved unassigned Source Bitmap', src);
    CheckAssigned('SDL1.3 ImagesDriver - RotateScaleSurfaceProcedure recieved empty Source Bitmap Surface', src^.surface);
        
    resultBmp^.surface := rotozoomSurface(GetSurface(src), deg, scale, 0);
    resultBmp^.width   := GetSurface(resultBmp)^.w;
    resultBmp^.height  := GetSurface(resultBmp)^.h;
	end;

	function SameBitmapProcedure(const bitmap1,bitmap2 : Bitmap) : Boolean;
	begin
	 result := (GetSurface(bitmap1) = GetSurface(bitmap2));
	end;
	
	procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
	var
	  sRect, dRect : SDL_Rect;
	  pDRect : ^SDL_Rect = nil;
	  pSRect : ^SDL_Rect = nil;
	  clearTexture : Boolean = False;
	  srcW : LongInt;
	begin
  // 	CheckAssigned('SDL1.3 ImagesDriver - BlitSurfaceProcedure recieved unassigned Source Bitmap', srcBmp);
  //  CheckAssigned('SDL1.3 ImagesDriver - BlitSurfaceProcedure recieved empty Source Bitmap Surface', srcBmp^.surface);
  //  CheckAssigned('SDL1.3 ImagesDriver - BlitSurfaceProcedure recieved unassigned Destination Bitmap', destBmp);
  //  CheckAssigned('SDL1.3 ImagesDriver - BlitSurfaceProcedure recieved empty Destination Bitmap Surface', destBmp^.surface);  
  //  if Assigned(GetSurface(srcBmp)) then
    srcW := srcBmp^.width;
    if Assigned(GetSurface(srcBmp)) and Assigned(srcRect) then
    begin
      srcW := GetSurface(srcBmp)^.w;
      if (srcRect^.x + srcRect^.width) > srcW then
      begin  
        srcRect^.width := srcW - LongInt(srcRect^.x);
        if (destRect^.width > srcW) then
          destRect^.width := srcW;
      end;
    end;
    
    if Assigned(destRect) and (destRect^.width > srcW) then
      destRect^.width := srcW;
    
	  if assigned(srcRect) then
	  begin
	    sRect := NewSDLRect(srcRect^);
  	  pSRect := @sRect;
	  end;
	  if assigned(destRect) then
	  begin
	    dRect := NewSDLRect(destRect^);
  	  pDRect := @dRect;
	  end;
	  
	  if not Assigned(PSDL13Surface(srcbmp^.surface)^.texture) then
	  begin
	    PSDL13Surface(srcbmp^.surface)^.texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, PSDL13Surface(srcbmp^.surface)^.surface);
	    clearTexture := True;
	  end;
	  
	  SDL_RenderCopy(PSDL13Screen(_screen)^.renderer, PSDL13Surface(srcbmp^.surface)^.texture, pSRect, pDRect);
	  
	  if clearTexture then
	  begin
	    SDL_DestroyTexture(PSDL13Surface(srcbmp^.surface)^.texture);
	    PSDL13Surface(srcbmp^.surface)^.texture:= nil;
	  end;
	  
	  
	end;
	
	procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color); 
	var
	  r, g, b, a : Byte;
	  x, y, w, h : LongInt;
  begin
 //  	CheckAssigned('SDL1.3 ImagesDriver - ClearSurfaceProcedure recieved empty Bitmap', dest);
//    CheckAssigned('SDL1.3 ImagesDriver - ClearSurfaceProcedure recieved empty Bitmap Surface', dest^.surface);
    if dest <> screen then
    begin
      SDL_FillRect(GetSurface(dest), @PSDL13Surface(dest^.surface)^.surface^.clip_rect, toColor);
      SDL_SetAlpha(GetSurface(dest), SDL_SRCALPHA, 255);
    end else begin
      GraphicsDriver.ColorComponents(toColor, r, g, b, a);
      SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
      SDL_RenderClear(PSDL13Screen(_screen)^.renderer);
    end;
  end;
  
	procedure OptimiseBitmapProcedure(surface : Bitmap); 
  var
    oldSurface: PSDL_Surface;
  begin
 	  CheckAssigned('SDL1.3 ImagesDriver - OptimiseBitmapProcedure recieved empty Bitmap', surface);
   	CheckAssigned('SDL1.3 ImagesDriver - OptimiseBitmapProcedure recieved empty Bitmap Surface', surface^.surface);
  
    oldSurface := GetSurface(surface);
    SetNonAlphaPixelsProcedure(surface);
    surface^.surface := SDL_DisplayFormatAlpha(oldSurface);
    SDL_FreeSurface(oldSurface);
  end;
  
  procedure SaveBitmapProcedure(src : Bitmap; filepath : String);
  begin
 	  CheckAssigned('SDL1.3 ImagesDriver - SaveBitmapProcedure recieved empty Bitmap', src);
   	CheckAssigned('SDL1.3 ImagesDriver - SaveBitmapProcedure recieved empty Bitmap Surface', src^.surface);   
    SDL_SaveBMP(GetSurface(src), PChar(filepath));
  end;
    
	procedure LoadSDL13ImagesDriver();
	begin
		ImagesDriver.InitBitmapColors                         := @InitBitmapColorsProcedure;
		ImagesDriver.SurfaceExists                            := @SurfaceExistsProcedure;
		ImagesDriver.CreateBitmap                             := @CreateBitmapProcedure;
		ImagesDriver.DoLoadBitmap                             := @DoLoadBitmapProcedure;
		ImagesDriver.FreeSurface                              := @FreeSurfaceProcedure;
		ImagesDriver.MakeOpaque                               := @MakeOpaqueProcedure;
		ImagesDriver.SetOpacity                               := @SetOpacityProcedure;
		ImagesDriver.SameBitmap                               := @SameBitmapProcedure;
		ImagesDriver.BlitSurface                              := @BlitSurfaceProcedure;
		ImagesDriver.MakeTransparent                          := @MakeTransparentProcedure;
		ImagesDriver.RotateScaleSurface                       := @RotateScaleSurfaceProcedure;
		ImagesDriver.ClearSurface                             := @ClearSurfaceProcedure;
		ImagesDriver.OptimiseBitmap                           := @OptimiseBitmapProcedure;
		ImagesDriver.SetNonAlphaPixels                        := @SetNonAlphaPixelsProcedure;
	end;
end.