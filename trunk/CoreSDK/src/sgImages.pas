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
// - 2009-12-07: Andrew : Added loading of image resources
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


//----------------------------------------------------------------------------
// Bitmap loading routines
//----------------------------------------------------------------------------
  
  /// Creates a bitmap in memory that is the specified width and height (in pixels).
  /// The new bitmap is initially transparent and can be used as the target 
  /// for various drawing operations. Once you have drawn the desired image onto
  /// the bitmap you can call OptimiseBitmap to optimise the surface.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithWidth:%s andHeight:%s
  function CreateBitmap(width, height: LongInt): Bitmap;

  /// Loads a bitmap from file using where the specified transparent color
  /// is used as a color key for the transparent color.
  ///
  /// @lib LoadBitmapWithTransparentColor
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithPath:%s withTransparency:%s usingColor:%s
  function LoadBitmap(filename: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;

  /// Loads a bitmap from file into a Bitmap variable. This can then be drawn to
  /// the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
  /// contain alpha values, which will be drawn correctly by the API. All
  /// bitmaps must be freed using the FreeBitmap once you are finished with
  /// them.
  /// 
  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithPath:%s
  function LoadBitmap(filename : String): Bitmap; overload;

  /// Loads a bitmap with a transparent color key. The transparent color is then
  /// setup as the color key to ensure the image is drawn correctly. Alpha
  /// values of Images loaded in this way will be ignored. All bitmaps must be
  /// freed using the FreeBitmap once you are finished with them.
  ///
  /// @lib LoadBitmapWithTransparentColor(filename, True, transparentColor)
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithPath:%s transparentColor:%s
  function LoadTransparentBitmap(filename : String; transparentColor : Color): Bitmap; overload;

  /// Frees a loaded bitmap. Use this when you will no longer be drawing the
  /// bitmap (including within Sprites), and when the program exits.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @dispose
  procedure FreeBitmap(var bitmapToFree : Bitmap);
  
  
//----------------------------------------------------------------------------
// Bitmap mapping routines
//----------------------------------------------------------------------------
  
  /// Loads and returns a bitmap. The supplied `filename` is used to
  /// locate the Bitmap to load. The supplied `name` indicates the 
  /// name to use to refer to this Bitmap in SwinGame. The `Bitmap` can then be
  /// retrieved by passing this `name` to the `FetchBitmap` function. 
  ///
  /// @lib
  ///
  /// @sn bitmapNamed:%s fromFilename:%s
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithName:%s forFilename:%s
  function MapBitmap(name, filename: String): Bitmap;
  
  /// Loads and returns a bitmap with a given color code use for transparency.
  /// The supplied `filename` is used to locate the Bitmap to load. The supplied
  /// `name` indicates thename to use to refer to this Bitmap in SwinGame. The 
  /// `Bitmap` can then be retrieved by passing this `name` to the `FetchBitmap` function. 
  ///
  /// @lib
  ///
  /// @sn bitmapNamed:%s fromFilename:%s withColorKey:%s
  ///
  /// @class Bitmap
  /// @constructor
  /// @csn initWithName:%s forFilename:%s andColorKey:%s
  function MapTransparentBitmap(name, filename: String; transparentColor: Color): Bitmap;
  
  /// Determines if SwinGame has a bitmap loaded for the supplied name.
  /// This checks against all bitmaps loaded, those loaded without a name
  /// are assigned the filename as a default.
  ///
  /// @lib
  function HasBitmap(name: String): Boolean;
  
  /// Returns the `Bitmap` that has been loaded with the specified name,
  /// see `MapBitmap`.
  ///
  /// @lib
  function FetchBitmap(name: String): Bitmap;
  
  /// Releases the SwinGame resources associated with the bitmap of the
  /// specified `name`.
  ///
  /// @lib
  procedure ReleaseBitmap(name: String);
  
  /// Releases all of the bitmaps that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllBitmaps();
  
  
//---------------------------------------------------------------------------
// Bitmap querying functions
//---------------------------------------------------------------------------
  
  /// Checks if a pixel is drawn at the specified x,y location.
  /// 
  /// @lib
  ///
  /// @class Bitmap
  /// @csn pixelDrawnAtX:%s y:%s
  /// @method PixelDrawnAtPoint
  function PixelDrawnAtPoint(bmp: Bitmap; x, y: LongInt): Boolean;
  
  
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
uses sgCore, sgShared, sgResources,
     stringhash,         // libsrc
     SysUtils,
     SDL_gfx, SDL, SDL_Image // sdl
     ;
//=============================================================================

var
  _Images: TStringHash;


//----------------------------------------------------------------------------

function CreateBitmap(width, height: LongInt): Bitmap;
var
  name: String;
  idx: LongInt;
  obj: tResourceContainer;
begin
  if (width < 1) or (height < 1) then
  begin
    RaiseException('Bitmap width and height must be greater then 0');
    exit;
  end;
  if (baseSurface = nil) or (baseSurface^.format = nil) then
  begin
    RaiseException('Unable to CreateBitmap as the window is not open');
    exit;
  end;
  
  New(result);
  
  with baseSurface^.format^ do
  begin
    result^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32,
                     RMask, GMask, BMask, AMask);
  end;
  
  if result^.surface = nil then
  begin
    Dispose(result);
    RaiseException('Failed to create a bitmap: ' + SDL_GetError());
    exit;
  end;
  
  //
  // Place the bitmap in the _Images hashtable
  //
  obj := tResourceContainer.Create(result);
  {$IFDEF TRACE}
    Trace('sgImages', 'Info', 'DoLoadBitmap', 'name = ' + name + 'obj = ' + HexStr(obj) + ' _Images = ' + HexStr(_Images));
  {$ENDIF}
  
  name := 'Bitmap';
  idx := 0;
  while not _Images.setValue(name, obj) do
  begin
    name := 'Bitmap_' + IntToStr(idx);
    idx := idx + 1;
  end;
  
  result^.width := width;
  result^.height := height;
  result^.cellCols  := 1;
  result^.cellRows  := 1;
  result^.cellCount := 1;
  result^.name      := name;
  result^.filename  := name;
  
  SDL_SetAlpha(result^.surface, SDL_SRCALPHA, 0);
  SDL_FillRect(result^.surface, nil, ColorTransparent);
end;

// Sets the non-transparent pixels in a Bitmap. This is then used for
// collision detection, allowing the original surface to be optimised.
//
// @param bmp  A pointer to the Bitmap being set
// @param surface The surface with pixel data for this Bitmap
procedure SetNonTransparentPixels(bmp: Bitmap; surface: PSDL_Surface; transparentColor: Color);
var
  r, c: LongInt;
begin
  SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

  for c := 0 to bmp^.width - 1 do
  begin
    for r := 0 to bmp^.height - 1 do
    begin
      bmp^.nonTransparentPixels[c, r] :=
        (GetPixel32(surface, c, r) <> transparentColor);
    end;
  end;
end;

function DoLoadBitmap(name, filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
var
  obj: tResourceContainer;
  loadedImage: PSDL_Surface;
  correctedTransColor: Color;
begin
  {$IFDEF TRACE}
    TraceEnter('sgImages', 'LoadBitmap', filename);
  {$ENDIF}
  
  result := nil; //start at nil to exit cleanly on error
  
  // Check for file
  if not FileExists(filename) then
  begin
    filename := PathToResource(filename, BitmapResource);
    
    if not FileExists(filename) then
    begin
      RaiseException('Unable to locate bitmap ' + filename);
      exit;
    end;
  end;
  
  //Load the image
  loadedImage := IMG_Load(PChar(filename));
  
  if loadedImage = nil then
  begin
    RaiseException('Error loading image: ' + filename + ': ' + SDL_GetError());
    exit;
  end;
  
  //
  // Image loaded, so create SwinGame bitmap
  //
  new(result);
  
  if not transparent then result^.surface := SDL_DisplayFormatAlpha(loadedImage)
  else result^.surface := SDL_DisplayFormat(loadedImage);
  
  result^.width     := result^.surface^.w;
  result^.height    := result^.surface^.h;
  result^.cellCols  := 1;
  result^.cellRows  := 1;
  result^.cellCount := 1;
  result^.name      := name;
  result^.filename  := filename;
  
  //Determine pixel level collision data
  if transparent then
  begin
    correctedTransColor := ColorFrom(result, transparentColor);
    SDL_SetColorKey(result^.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, correctedTransColor);
    SetNonTransparentPixels(result, loadedImage, correctedTransColor);
  end
  else
  begin
    SetNonAlphaPixels(result, loadedImage);
  end;
  
  // Free the loaded image if its not the result's surface
  if loadedImage <> result^.surface then SDL_FreeSurface(loadedImage);
  
  //
  // Place the bitmap in the _Images hashtable
  //
  obj := tResourceContainer.Create(result);
  {$IFDEF TRACE}
    Trace('sgImages', 'Info', 'DoLoadBitmap', 'name = ' + name + 'obj = ' + HexStr(obj) + ' _Images = ' + HexStr(_Images));
  {$ENDIF}
  if not _Images.setValue(name, obj) then
  begin
    FreeBitmap(result);
    RaiseException('Error loaded Bitmap resource twice: ' + name + ' for file ' + filename);
    exit;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgImages', 'LoadBitmap, result = ' + HexStr(result));
  {$ENDIF}
end;

function LoadBitmap(filename: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
begin
  result := DoLoadBitmap(filename + IntToStr(transparentColor),filename, transparent, transparentColor);
end;

function LoadBitmap(filename: String): Bitmap; overload;
begin
  result := DoLoadBitmap(filename, filename, false, ColorBlack);
end;

function LoadTransparentBitmap(filename: String; transparentColor: Color): Bitmap; overload;
begin
  result := LoadBitmap(filename, true, transparentColor);
end;

// private:
// Called to actually free the resource
procedure DoFreeBitmap(var bitmapToFree : Bitmap);
begin
  if Assigned(bitmapToFree) then
  begin
    if Assigned(bitmapToFree^.surface) then
    begin
      //WriteLn('Free Bitmap - ', HexStr(bitmapToFree^.surface));
      SDL_FreeSurface(bitmapToFree^.surface);
    end;
    bitmapToFree^.surface := nil;
    
    Dispose(bitmapToFree);
    CallFreeNotifier(bitmapToFree);
    bitmapToFree := nil;
  end;
end;

procedure FreeBitmap(var bitmapToFree : Bitmap);
begin
  {$IFDEF TRACE}
    TraceEnter('sgImages', 'FreeBitmap', 'effect = ' + HexStr(bitmapToFree));
  {$ENDIF}
  
  if(assigned(bitmapToFree)) then
  begin
    ReleaseBitmap(bitmapToFree^.name);
  end;
  bitmapToFree := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgImages', 'FreeBitmap');
  {$ENDIF}
end;


//----------------------------------------------------------------------------

function MapBitmap(name, filename: String): Bitmap;
begin
  {$IFDEF TRACE}
    TraceEnter('sgImages', 'MapBitmap', name + ' -> ' + filename);
  {$ENDIF}
  
  result := DoLoadBitmap(name, filename, false, ColorBlack);
  
  {$IFDEF TRACE}
    TraceExit('sgImages', 'MapBitmap');
  {$ENDIF}
end;

function MapTransparentBitmap(name, filename: String; transparentColor: Color): Bitmap;
begin
  {$IFDEF TRACE}
    TraceEnter('sgImages', 'MapBitmap', name + ' -> ' + filename);
  {$ENDIF}
  
  result := DoLoadBitmap(name, filename, true, transparentColor);
  
  {$IFDEF TRACE}
    TraceExit('sgImages', 'MapBitmap');
  {$ENDIF}
end;

function HasBitmap(name: String): Boolean;
begin
  result := _Images.containsKey(name);
end;

function FetchBitmap(name: String): Bitmap;
var
  tmp : TObject;
begin
  tmp := _Images.values[name];
  if assigned(tmp) then
    result := Bitmap(tResourceContainer(tmp).Resource)
  else 
    result := nil;
end;

procedure ReleaseBitmap(name: String);
var
  bmp: Bitmap;
begin
  bmp := FetchBitmap(name);
  if (assigned(bmp)) then
  begin
    _Images.remove(name).Free();
    FreeBitmap(bmp);
  end;
end;

procedure ReleaseAllBitmaps();
begin
  ReleaseAll(_Images, @ReleaseBitmap);
end;

//----------------------------------------------------------------------------

function PixelDrawnAtPoint(bmp: Bitmap; x, y: LongInt): Boolean;
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

  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgImages', 'initialization');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _Images := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgImages', 'initialization');
    {$ENDIF}
  end;

//=============================================================================
end.
//=============================================================================
