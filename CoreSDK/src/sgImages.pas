//=============================================================================
// sgImages.pas
//=============================================================================
//
// The Images unit contains the code related to manipulating and querying
// bitmap structures.
//
// Change History:
//
// Version 3.0:
// - 2009-11-6: Andrew  : Started Images unit.
//
//=============================================================================

/// The Images module contains the code that relates to the manipulating and
/// querying of bitmap structures.
///
/// @module Images
/// @static
unit sgImages;

//=============================================================================
interface
uses sgTypes;
//=============================================================================


  /// @lib
  function IsPixelDrawnAtPoint(bmp: Bitmap; x, y: LongInt): Boolean;

//---------------------------------------------------------------------------
// Alpha blendings adjusting code
//---------------------------------------------------------------------------

  /// @lib
  /// @class Bitmap
  /// @method MakeOpaque
  procedure MakeOpaque(bmp: Bitmap);
  /// @lib
  /// @class Bitmap
  /// @method MakeTransparent
  procedure MakeTransparent(bmp: Bitmap);

//---------------------------------------------------------------------------
// Rotate and Zoom
//---------------------------------------------------------------------------

  /// @lib
  /// @class Bitmap
  /// @method RotateScaleBitmap
  function RotateScaleBitmap(src: Bitmap; degRot, scale: Single): Bitmap;

  /// @lib
  /// @class Bitmap
  /// @method SetupForCollisions
  procedure SetupBitmapForCollisions(src: Bitmap);

//---------------------------------------------------------------------------
// Optimise
//---------------------------------------------------------------------------

  /// Created bitmaps can be optimised for faster drawing to the screen. This
  /// optimisation should be called only once after all drawing to the bitmap
  /// is complete. Optimisation should not be used if the bitmap is to be
  /// drawn onto in the future. All loaded bitmaps are optimised during loading.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @method OptimiseBitmap
  procedure OptimiseBitmap(surface: Bitmap);



//=============================================================================
implementation
uses sgCore, sgShared,
     SDL_gfx, SDL, SDL_Image // sdl
     ;
//=============================================================================

function IsPixelDrawnAtPoint(bmp: Bitmap; x, y: LongInt): Boolean;
begin
  result := (Length(bmp^.nonTransparentPixels) = bmp^.width)
            and ((x >= 0) and (x < bmp^.width))
            and ((y >= 0) and (y < bmp^.height))
            and bmp^.nonTransparentPixels[x, y];
end;

//---------------------------------------------------------------------------

procedure MakeOpaque(bmp: Bitmap);
begin
  SDL_SetAlpha(bmp^.surface, 0, 255);
end;

procedure MakeTransparent(bmp: Bitmap);
begin
  SDL_SetAlpha(bmp^.surface, SDL_SRCALPHA, 0);
end;

//---------------------------------------------------------------------------

function RotateScaleBitmap(src: Bitmap; degRot, scale: Single): Bitmap;
begin
  New(result);
  result^.surface := rotozoomSurface(src^.surface, degRot, scale, 1);
  result^.width := result^.surface^.w;
  result^.height := result^.surface^.h;
end;

procedure SetupBitmapForCollisions(src: Bitmap);
begin
  if Length(src^.nonTransparentPixels) <> 0 then exit;
    
  SetNonAlphaPixels(src, src^.surface);
  OptimiseBitmap(src);
end;

//---------------------------------------------------------------------------

procedure OptimiseBitmap(surface: Bitmap);
var
  oldSurface: PSDL_Surface;
begin
  if surface = nil then
  begin
    RaiseException('No bitmap supplied');
    exit;
  end;
  
  oldSurface := surface^.surface;
  SetNonAlphaPixels(surface, oldSurface);
  surface^.surface := SDL_DisplayFormatAlpha(oldSurface);
  SDL_FreeSurface(oldSurface);
end;

//=============================================================================
end.
//=============================================================================
