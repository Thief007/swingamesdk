unit sgDriverImagesSDL;
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
	
	procedure LoadSDLImagesDriver();
		
implementation
	uses sgDriverImages, sgShared, sgTypes, SysUtils, sgGraphics,
	     SDL_gfx, SDL, SDL_Image; // sdl;
	
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
 	begin	  
     SDL_SetAlpha(bmp^.surface, SDL_SRCALPHA, 0);
     SDL_FillRect(bmp^.surface, nil, ColorTransparent);
 	end;
     	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  result := Assigned(bmp^.surface);
	end;
	
	procedure CreateBitmapProcedure(bmp : Bitmap; width, height : LongInt);
	begin
		if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      RaiseWarning('Creating ARGB surface as screen format unknown.');
      bmp^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, $00FF0000, $0000FF00, $000000FF, $FF000000);
    end
    else
    begin
      with baseSurface^.format^ do
      begin
        bmp^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, RMask, GMask, BMask, AMask);
      end;
    end;
	end;

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; surface: PSDL_Surface; transparentColor: Color);
  var
    r, c: Longint;
  begin
    if not assigned(bmp) then exit;

    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := (GetPixel32(surface, c, r) <> transparentColor);
      end;
    end;
  end;
	
	function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  var
    loadedImage: PSDL_Surface;
    correctedTransColor: Color;
  begin
    result := nil; //start at nil to exit cleanly on error
    
    //Load the image
    loadedImage := IMG_Load(PChar(filename));

    if loadedImage = nil then
    begin
      RaiseWarning('Error loading image: ' + filename + ': ' + SDL_GetError());
      exit;
    end;

    //
    // Image loaded, so create SwinGame bitmap
    //
    new(result);

    if not transparent then 
    begin
      // Only move to screen alpha if window is open...
      if baseSurface = nil then
        result^.surface := loadedImage
      else
        result^.surface := SDL_DisplayFormatAlpha(loadedImage);
    end
    else result^.surface := SDL_DisplayFormat(loadedImage);

    if not assigned(result^.surface) then
    begin
      SDL_FreeSurface(loadedImage);
      dispose(result);
      result := nil;
      RaiseException('Error loading image.')
    end;

    result^.width     := result^.surface^.w;
    result^.height    := result^.surface^.h;

    //Determine pixel level collision data
    if transparent then
    begin
      correctedTransColor := ColorFrom(result, transparentColor);
      SDL_SetColorKey(result^.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, correctedTransColor);
      SetNonTransparentPixels(result, loadedImage, transparentColor);
    end
    else
    begin
      SetNonAlphaPixels(result, loadedImage);
    end;

    // Free the loaded image if its not the result's surface
    if loadedImage <> result^.surface then SDL_FreeSurface(loadedImage);
	end;
	
	function SameBitmapProcedure(const bitmap1,bitmap2 : Bitmap) : Boolean;
	begin
	 result := (Bitmap1^.surface = Bitmap2^.surface);
	end;
	
	procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
	var
	  sRect, dRect : SDL_Rect;
	  pDRect : ^SDL_Rect = nil;
	  pSRect : ^SDL_Rect = nil;
	   
	begin
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
	  
	  SDL_BlitSurface(srcBmp^.surface, pSRect, destBmp^.surface, pDRect)
	end;
  
	procedure LoadSDLImagesDriver();
	begin
		Write('Loading SDL 1.2 Driver...');
		ImagesDriver.InitBitmapColors                         := @InitBitmapColorsProcedure;
		ImagesDriver.SurfaceExists                            := @SurfaceExistsProcedure;
		ImagesDriver.CreateBitmap                             := @CreateBitmapProcedure;
		ImagesDriver.DoLoadBitmap                             := @DoLoadBitmapProcedure;
		ImagesDriver.SameBitmap                               := @SameBitmapProcedure;
		ImagesDriver.BlitSurface                              := @BlitSurfaceProcedure;
		WriteLn('Finished.');
	end;
end.