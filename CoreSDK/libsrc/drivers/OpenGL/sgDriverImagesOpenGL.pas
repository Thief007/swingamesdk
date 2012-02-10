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
  
  procedure LoadOpenGLImagesDriver();
    
implementation
  uses sgTypes,
       sgDriverGraphics, sdl13, sgShared, gl, sgDriverImages; // sdl;
    
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
  
  function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  begin
    result := nil; //start at nil to exit cleanly on error
  end; 
  
  procedure FreeSurfaceProcedure(bmp : Bitmap);
  begin   
    exit;
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
  
  procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
  begin
    exit;   
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