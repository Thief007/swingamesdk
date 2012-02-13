unit sgDriverImagesOpenGL;
//=============================================================================
// sgDriverImagesSDL13.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//    - Pascal PChar is equivalent to a C-type string
//    - Pascal Word is equivalent to a Uint16
//    - Pascal LongWord is equivalent to a Uint32
//    - Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
  uses glu, glut, glext, gl;

  type
    PTexture = ^GLUint;
  
  procedure LoadOpenGLImagesDriver();
    
implementation
  uses sgTypes,
       sgDriverGraphics, sdl13, sgShared, sgDriverImages, sdl13_image, SysUtils, sgSharedUtils; // sdl;
    
  procedure InitBitmapColorsProcedure(bmp : Bitmap);
  begin   
    exit;
  end;
      
  function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
  begin
    result := false;
  end;
  

  procedure CreateBitmapProcedure(bmp : Bitmap; width, height : LongInt);
  begin   
    exit;
  end;

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; transparentColor: Color);
  begin   
    exit;
  end;
  
  procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
  begin   
    exit;
  end;
  
  function CreateGLTextureFromSurface(lLoadedImg : PSDL_Surface) : PTexture;
  var
    lFormat : GLenum;
    lNoOfColors : GLint;
  begin
    result := nil;
    New(result);
    if (Assigned(lLoadedImg) ) then
    begin     
      // Check that the image's width is a power of 2
      if ( lLoadedImg^.w  = 0 ) or ( lLoadedImg^.h = 0 ) then
      begin
        WriteLn('BadStuff');
      end;

      // get the number of channels in the SDL surface
      lNoOfColors := lLoadedImg^.format^.BytesPerPixel;
      if (lNoOfColors = 4) then    // contains an alpha channel
      begin
        if (lLoadedImg^.format^.Rmask = $000000FF) then
          lFormat := GL_RGBA
        else
          lFormat := GL_BGRA
      end else if (lNoOfColors = 3) then     // no alpha channel
      begin
        if (lLoadedImg^.format^.Rmask = $000000FF) then
          lFormat := GL_RGB
        else
          lFormat := GL_BGR;
      end;    
 
      // Have OpenGL generate a texture object handle for us
      glGenTextures( 1, @result^ );
     
      // Bind the texture object
      glBindTexture( GL_TEXTURE_2D, result^ );
      glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     
      // Set the texture's stretching properties
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
     
      // Edit the texture object's image data using the information SDL_Surface gives us
      glTexImage2D( GL_TEXTURE_2D, 0, lNoOfColors, lLoadedImg^.w, lLoadedImg^.h, 0,
                          lFormat, GL_UNSIGNED_BYTE, lLoadedImg^.pixels );
      glBindTexture( GL_TEXTURE_2D, 0 );
    end;
  end; 
  
  procedure PutSurfacePixel(surf : PSDL_Surface; clr: Color; x, y: Longint);
  var
      p:    ^Color;
      bpp:  Longint;
  begin
      if not Assigned(surf) then begin RaiseWarning('OpenGL Images Driver - PutPixelProcedure recieved empty Surface'); exit; end;
        
      bpp := surf^.format^.BytesPerPixel;
      // Here p is the address to the pixel we want to set
      p := surf^.pixels + y * surf^.pitch + x * bpp;

      if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
      p^ := clr;
  end; 

  function GetSurfacePixel(surface: PSDL_Surface; x, y: Longint) :Color;
  var
    pixel, pixels: PUint32;
    offset: Longword;
  {$IFDEF FPC}
    pixelAddress: PUint32;
  {$ELSE}
    pixelAddress: Longword;
  {$ENDIF}
  begin    
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
    end;
    {$IFEND}
  end;

  procedure ReplaceColors(surf : PSDL_Surface; originalColor, newColor : Color; width, height : LongInt);
  var
    x, y : LongInt;
  begin
    for x := 0 to width - 1  do 
    begin
      for y := 0 to height - 1 do 
      begin
        if GetSurfacePixel(surf, x, y) = originalColor then
          PutSurfacePixel(surf, newColor, x, y);
      end;
    end;
  end;

  function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  var
    loadedImage: PSDL_Surface;
    offset : Rectangle;
    lTransparentSurface : PSDL_Surface;
    lColorToSetTo : Color = $00000000;
  begin
    result := nil; //start at nil to exit cleanly on error
    
    //Load the image
    loadedImage := IMG_Load(PChar(filename));

    CheckAssigned('OpenGL ImagesDriver - Error loading image: ' + filename + ': ' + SDL_GetError(), loadedImage);

    // Image loaded, so create SwinGame bitmap    
    new(result);
    result^.width     := loadedImage^.w;
    result^.height    := loadedImage^.h;   
     
    //Determine pixel level collision data
    if transparent then
    begin
      offset.x := 0;
      offset.y := 0;
      offset.width := result^.width;
      offset.height := result^.height;
      ReplaceColors(loadedImage, transparentColor, lColorToSetTo, result^.width, result^.height);

      result^.surface := CreateGLTextureFromSurface(loadedImage);
    end else begin    
      SetNonAlphaPixelsProcedure(result);
    end;

    // Free the loaded image; if its not the result's surface
    result^.surface := CreateGLTextureFromSurface(loadedImage);
    SDL_FreeSurface(loadedImage);
  end; 
    
  procedure FreeSurfaceProcedure(bmp : Bitmap);
  begin   
    if (Assigned(bmp) and Assigned(bmp^.surface)) then
    begin
      Dispose(PTexture(bmp^.surface));
      bmp^.surface := nil;
    end;
  end; 
  
  procedure MakeOpaqueProcedure(bmp : Bitmap);
  begin   
    exit;
  end;

  procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
  begin   
    exit;
  end;

  procedure MakeTransparentProcedure(bmp : Bitmap);
  begin   
    exit;
  end;

  procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
  begin   
    exit;
  end;

  function SameBitmapProcedure(const bitmap1,bitmap2 : Bitmap) : Boolean;
  begin
   result := false;
  end;

  function GetCoords(bmpSize : LongInt; srcRectSize : Single) : Single;
  begin
    result := 0;
    if (bmpSize = 0) or (srcRectSize = 0) then
      exit
    else
      result := srcRectSize / bmpSize;
  end;
  
  procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
  var
    lTexture : GLuint;
    lRatioX, lRatioY, lRatioW, lRatioH : Single;
  begin
    if (srcRect = nil) then
    begin
      lRatioX := 0;
      lRatioY := 0;
      lRatioW := 1;
      lRatioH := 1;
    end else begin
      lRatioX := GetCoords(srcBMP^.width, srcRect^.x);
      lRatioY := GetCoords(srcBMP^.height, srcRect^.y);
      lRatioW := lRatioX + GetCoords(srcBMP^.width, srcRect^.width);
      lRatioH := lRatioY + GetCoords(srcBMP^.height, srcRect^.height);
    end;
 //   if (destBmp <> nil) then exit;
    glBindTexture( GL_TEXTURE_2D, GLUInt(srcBmp^.surface^) );
 
    glBegin( GL_QUADS );
      //Bottom-left vertex (corner)
      glTexCoord2f( lRatioX, lRatioY );
      glVertex2f( destRect^.x, destRect^.y );
     
      //Bottom-right vertex (corner)
      glTexCoord2f( lRatioW, lRatioY );
      glVertex2f( destRect^.x + destRect^.width, destRect^.y);
     
      //Top-right vertex (corner)
      glTexCoord2f( lRatioW, lRatioH );
      glVertex2f( destRect^.x + destRect^.width, destRect^.y + destRect^.height);
     
      //Top-left vertex (corner)
      glTexCoord2f( lRatioX, lRatioH );
      glVertex2f( destRect^.x , destRect^.y + destRect^.height );
    glEnd();
    glBindTexture( GL_TEXTURE_2D, 0 );
  end;
  
  procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color); 
  var
    r,g,b,a : Byte;
  begin   
    if dest <> Screen then
    begin
      
    end
    else
    begin
      GraphicsDriver.ColorComponents(toColor,r,g,b,a);
      glClearColor ( r/255,g/255,b/255,a/255 );
      glClear ( GL_COLOR_BUFFER_BIT );
    end;

  end;
  
  procedure OptimiseBitmapProcedure(surface : Bitmap); 
  begin   
    exit;
  end;
  
  procedure SaveBitmapProcedure(src : Bitmap; filepath : String);
  begin   
    exit;
  end;
    
  procedure LoadOpenGLImagesDriver();
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