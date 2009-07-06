//=============================================================================
// sgSprites.pas
//=============================================================================
//
// Create and manage sprites in SwinGame.
//
// Change History:
//
// Version 3.0:
// - 2009-07-06: Andrew : Added property access routines for Sprite data
// - 2009-06-29: Andrew : Renamed CurrentWidth to SpriteWidth
//                      : Renamed CurrentHeight to SpriteHeight
//                      : Renamed IsSpriteOnScreenAt to SpriteOnScreenAt
// - 2009-06-20: Andrew : Created Sprites unit.
//
//=============================================================================

/// Sprite management code.
///
/// @module Sprites
unit sgSprites;

//=============================================================================
interface
//=============================================================================

  uses sgTypes;

  /// Returns a `Vector` that is the difference in the position of two sprites
  /// (``s1`` and ``s2``).
  ///
  /// @lib
  /// @class Sprite
  /// @method VectorTo
  function VectorFromTo(s1, s2: Sprite): Vector;

  /// Returns a `Vector` that is the difference in location from the center of
  /// the sprite ``s`` to the point ``pt``.
  ///
  /// @lib
  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;


  //---------------------------------------------------------------------------
  // Sprite routines
  //---------------------------------------------------------------------------
  // These routines are used to work with Sprites within your game.
  //

  /// @lib CreateAnimatedCellSpriteWithEndingAction
  /// @class Sprite
  /// @constructor
  function CreateSprite(bmp : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; endingAction : SpriteEndingAction; width, height : LongInt): Sprite; overload;
  
  /// @lib CreateAnimatedCellSprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(bmp : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; width, height : LongInt): Sprite; overload;
  
  /// @lib CreateAnimatedCellSpriteWithSetFramesPerCell
  /// @class Sprite
  /// @constructor
  function CreateSprite(bmp : Bitmap; framesPerCell, frames, width, height: LongInt): Sprite; overload;
  
  /// @lib CreateBasicSprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(bmp : Bitmap): Sprite; overload;
  
  /// @lib CreateAnimatedArraySpriteWithEndingAction
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray; endingAction : SpriteEndingAction): Sprite; overload;
  
  /// @lib CreateAnimatedArraySprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray): Sprite; overload;
  
  /// @lib CreateAnimatedArraySpriteWithFramesPerCell
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; framesPerCell, frames : LongInt): Sprite; overload;

  /// @lib
  /// @class Sprite
  /// @dispose
  procedure FreeSprite(var s : Sprite);

  /// @lib
  /// @class Sprite
  /// @method AddBitmap
  function AddBitmapToSprite(s: Sprite; bitmapToAdd: Bitmap): LongInt;

  /// @lib
  /// @class Sprite
  /// @getter Height
  function SpriteHeight(s: Sprite): LongInt;
  /// @lib
  /// @class Sprite
  /// @getter Width
  function SpriteWidth(s: Sprite): LongInt;

  /// @lib
  /// @class Sprite
  /// @method ReplayAnimation
  procedure ReplayAnimation(s : Sprite);

  /// @lib UpdateSpritePct(s, 1.0)
  /// @uname UpdateSprite
  /// @class Sprite
  /// @method Update
  procedure UpdateSprite(s: Sprite); overload;
  /// @lib UpdateSpritePct
  /// @class Sprite
  /// @overload Update UpdatePct
  procedure UpdateSprite(s: Sprite; pct: Single); overload;

  /// @lib UpdateSpriteAnimationPct(s, 1.0)
  /// @uname UpdateSpriteAnimation
  /// @class Sprite
  /// @method UpdateAnimation
  procedure UpdateSpriteAnimation(s: Sprite); overload;
  /// @lib UpdateSpriteAnimationPct
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationPct
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;

  /// @lib DrawSpriteOffsetXY
  /// @class Sprite
  /// @overload Draw DrawOffsetXY
  procedure DrawSprite(s : Sprite; xOffset, yOffset: LongInt); overload;
  /// @lib DrawSpriteOffsetPoint
  /// @class Sprite
  /// @overload Draw DrawOffsetPoint
  procedure DrawSprite(s : Sprite; const position: Point2D); overload;
  /// @lib DrawSpriteOffsetXY(s, 0, 0)
  /// @uname DrawSprite
  /// @class Sprite
  /// @method Draw
  procedure DrawSprite(s : Sprite); overload;
  
  /// @lib MoveSprite(s, 1.0)
  /// @uname MoveSprite
  /// @class Sprite
  /// @method Move
  procedure MoveSprite(s: Sprite); overload;
  /// @lib MoveSprite
  /// @uname MoveSpritePct
  /// @class Sprite
  /// @overload Move MovePct
  procedure MoveSprite(s: Sprite; pct: Single); overload;
  /// @lib MoveSpriteVecPct(s, movementVector, 1.0)
  /// @uname MoveSpriteVec
  /// @class Sprite
  /// @overload Move MoveVec
  procedure MoveSprite(s : Sprite; const movementVector: Vector); overload;
  /// @lib MoveSpriteVecPct
  /// @class Sprite
  ///@overload Move MoveVecPct
  procedure MoveSprite(s : Sprite; const movementVector: Vector; pct: Single); overload;

  /// @lib
  /// @class Sprite
  /// @method MoveTo
  procedure MoveSpriteTo(s : Sprite; x,y : LongInt);
  
  //---------------------------------------------------------------------------
  // Sprite Screen Position Tests 
  //---------------------------------------------------------------------------

  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``x`` and ``y``) which is converted to a world location.
  ///
  /// @lib
  /// @class Sprite
  /// @method OnScreenAt
  function SpriteOnScreenAt(s: Sprite; x, y: LongInt): Boolean; overload;

  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``pt``), which is converted to a world location.
  ///
  /// @lib SpriteOnScreenAtPoint
  /// @class Sprite
  /// @overload OnScreenAt OnScreenAtPoint
  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;

  //---------------------------------------------------------------------------

  /// @lib
  /// @class Sprite
  /// @method IsOffscreen
  function IsSpriteOffscreen(s : Sprite): Boolean;

  //---------------------------------------------------------------------------
  // Sprite movement
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @getter Movement
  function SpriteMovement(s: Sprite): Vector;

  /// @lib
  /// @class Sprite
  /// @setter Movement
  procedure SetSpriteMovement(s: Sprite; const value: Vector);

  //---------------------------------------------------------------------------
  // Sprite CellCount
  //---------------------------------------------------------------------------

  /// @lib
  /// @class Sprite
  /// @getter CellCount
  function SpriteCellCount(s: Sprite): LongInt;
  
  //---------------------------------------------------------------------------
  // Sprite frames per cell
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @setter FramesPerCell
  /// @length SpriteCellCount
  procedure SetSpriteFramesPerCell(s: Sprite; const arr: LongIntArray);
  
  /// @lib
  /// @class Sprite
  /// @getter FramesPerCell
  /// @length SpriteCellCount
  function SpriteFramesPerCell(s: Sprite): LongIntArray;
  

//=============================================================================
implementation
//=============================================================================

  uses
    Classes, SysUtils, Math, // System
    sgGraphics, sgGeometry, sgCore, sgCamera; //SwinGame


  function VectorFromTo(s1, s2: Sprite): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s1), CenterPoint(s2));
  end;

  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s), pt);
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param startBitmap:   The sprites first bitmap (index 0)
  /// @param isMulti:     True if the bitmap specified is a multi bitmap
  /// @param framesPerCell: Array of LongInt that defines the frames per cell
  /// @param endingAction:  This sprite's ending action (Loop, ReverseLoop or Stop)
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(bmp : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray;
    endingAction : SpriteEndingAction; width, height : LongInt): Sprite; overload;
  var
    i : LongInt;
  begin
    if bmp = nil then raise Exception.Create('No image specified to create a sprite');
    if isMulti and (Length(framesPerCell) = 0) then raise Exception.Create('No frames per cell defined');
    if (width < 1) or (height < 1) then raise Exception.Create('Sprite Width and Height must be greater then 0');

    New(result);
    SetLength(result^.bitmaps, 1);
  
    if isMulti then
    begin
      result^.spriteKind := AnimMultiSprite;
    
      result^.cols := bmp^.width div width;
      result^.row := bmp^.height div height;
    
      SetLength(result^.framesPerCell, Length(framesPerCell));
      for i := 0 to High(framesPerCell) do
      begin
        if framesPerCell[i] < 0 then 
          raise Exception.Create('Frames per cell must be larger than 0');
      
        result^.framesPerCell[i] := framesPerCell[i];
      end;
    end
    else
    begin
      result^.spriteKind := StaticSprite;
    end;

    result^.x              := 0;
    result^.y              := 0;
    // result^.xPos            := @result^.x;
    // result^.yPos            := @result^.y;
    result^.currentFrame   := 0;
    result^.usePixelCollision  := true;
    result^.hasEnded       := false;
    result^.bitmaps[0]     := bmp;
    result^.frameCount     := 0;
    result^.endingAction   := endingAction;
    result^.width          := width;
    result^.height         := height;
    result^.reverse        := false;
    result^.movement       := VectorFrom(0,0);
    result^.rotation       := 0;
    result^.zoom           := 1;
    result^.bufferedRotation := 0;
    result^.bufferedZoom   := 1;
    result^.bufferBmp      := nil;
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param bmp:     The sprites first bitmap (index 0)
  /// @param isMulti:     True if the bitmap specified is a multi bitmap
  /// @param framesPerCell: Array of LongInt that defines the frames per cell
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite
  function CreateSprite(bmp : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; 
    width, height : LongInt): Sprite; overload;
  begin
    result := CreateSprite(bmp, isMulti, framesPerCell, Loop, width, height);
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param bmp:   The sprites first bitmap (index 0)
  /// @param framesPerCell: Number of frames per cell
  /// @param frames:      Number of frames this sprite contains
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite
  function CreateSprite(bmp: Bitmap; framesPerCell, frames, width, height: LongInt): Sprite; overload;
  var
    tempIntegers: LongIntArray;
    i: LongInt;
  begin
    if framesPerCell <= 0 then raise Exception.Create('Frames per cell must be larger than 0');

    SetLength(tempIntegers, frames);
    for i := 0 to High(tempIntegers) do
    begin
      tempIntegers[i] := framesPerCell;
    end;
    result := CreateSprite(bmp, true, tempIntegers, width, height);
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param bmp:     The sprites first bitmap (index 0)
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(bmp : Bitmap): Sprite; overload;
  var
    empty : LongIntArray;
  begin
    SetLength(empty, 0);
    result := CreateSprite(bmp, false, empty, bmp^.width, bmp^.height);
  end;

  /// Creates a sprites ans set bitmaps.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Array of Integer that defines the frames per cell
  /// @param endingAction:  Ending action of this sprite when it finishes animating
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(const bitmaps: BitmapArray; const framesPerCell: LongIntArray; endingAction: SpriteEndingAction): Sprite; overload;
  var
    i : LongInt;
  begin
    if Length(bitmaps) = 0 then raise Exception.Create('No images specified to create a sprite');
    if Length(framesPerCell) = 0 then raise Exception.Create('No frames per cell defined');
  
    New(result);
    result^.x          := 0;
    result^.y          := 0;
    // result^.xPos          := @result^.x;
    // result^.yPos          := @result^.y;
    result^.currentFrame     := 0;
    result^.usePixelCollision  := true;
    result^.hasEnded       := false;
    result^.movement       := VectorFrom(0,0);
    result^.rotation       := 0;
    result^.zoom           := 1;
    result^.bufferedRotation := 0;
    result^.bufferedZoom   := 1;
    result^.bufferBmp      := nil; 
    result^.endingAction     := endingAction;
    result^.width        := bitmaps[0]^.width;
    result^.height       := bitmaps[0]^.height;
    result^.reverse        := false;
    result^.spriteKind     := AnimArraySprite;

    SetLength(result^.bitmaps, Length(bitmaps));
    for i := 0 to High(bitmaps) do
    begin
      result^.bitmaps[i] := bitmaps[i];
    end;

    SetLength(result^.framesPerCell, Length(framesPerCell));
    for i := 0 to High(framesPerCell) do
    begin
      if framesPerCell[i] <= 0 then 
        raise Exception.Create('Frames per cell must be larger than 0');

      result^.framesPerCell[i] := framesPerCell[i];
    end;
  end;

  /// Creates a sprites ans set bitmaps.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Array of Integer that defines the frames per cell
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray): Sprite; overload;
  begin
    result := CreateSprite(bitmaps, framesPerCell, Loop);
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Number of frames per cell
  /// @param frames:      Number of frames this sprite contains
  /// @returns:       A new sprite
  function CreateSprite(const bitmaps: BitmapArray; framesPerCell, frames: LongInt): Sprite; overload;
  var
    tempIntegers: LongIntArray;
    i: LongInt;
  begin
    if framesPerCell <= 0 then raise Exception.Create('Frames per cell must be larger than 0');

    SetLength(tempIntegers, frames);
    for i := 0 to High(tempIntegers) do
    begin
      tempIntegers[i] := framesPerCell;
    end;

    result := CreateSprite(bitmaps, tempIntegers);
  end;

  procedure UpdateSpriteBuffers(s: Sprite);
  var
    dest: Bitmap; //temporary surface
    srcX, srcY: LongInt; //for image parts
  begin
    if (s^.rotation = s^.bufferedRotation) and (s^.zoom = s^.bufferedZoom) then exit;
    if (s^.bufferBmp <> nil) then FreeBitmap(s^.bufferBmp);
    if (s^.rotation = 0) and (s^.zoom = 1) then exit; //no need to transform

    //Draw non-transformed bitmap onto temp surface
    dest := CreateBitmap(s^.width, s^.height);
  
    if s^.spriteKind <> AnimMultiSprite then
      DrawBitmap(dest, s^.bitmaps[s^.currentFrame], 0, 0)
    else
    begin
      with s^ do
      begin
        srcX := (currentFrame mod cols) * width;
        srcY := (currentFrame - (currentFrame mod cols)) div cols * height;
      end;

      MakeOpaque(s^.bitmaps[0]);
      DrawBitmapPart(dest, s^.bitmaps[0], srcX, srcY, s^.width, s^.height, 0, 0);
      MakeTransparent(s^.bitmaps[0]);
    end;
  
    s^.bufferBmp := RotateScaleBitmap(dest, s^.rotation, s^.zoom);

    FreeBitmap(dest);
  end;

  procedure FreeSprite(var s : Sprite);
  begin
    if Assigned(s) then
    begin
      //Free bitmaps
      SetLength(s^.bitmaps, 0);
    
      //Free buffered rotation image
      if s^.bufferBmp <> nil then FreeBitmap(s^.bufferBmp);
      s^.bufferBmp := nil;
    
      //Dispose sprite
      Dispose(s);
      s := nil;
    end;
  end;

  /// Sprites may contain multiple images. These images can be used for things
  /// line animation, facing, etc. This routine adds a bitmap to a sprite,
  /// returning the index of the added bitmap.
  ///
  /// @param s:   the sprite to add the bitmap to
  /// @param bitmapToAdd:     the bitmap to add to the sprite
  /// @returns :               the index of the added bitmap
  ///
  /// Side Effects:
  /// - The bitmaps is added to the bitmaps within the sprite.
  function AddBitmapToSprite(s : Sprite; bitmapToAdd : Bitmap): LongInt;
  begin
    if bitmapToAdd = nil then raise Exception.Create('Cannot add non-existing bitmap to Sprite');
    if s = nil then raise Exception.Create('No sprite to add to');
    if s^.spriteKind = AnimMultiSprite then raise Exception.Create('Cannot add bitmap to an animated multi-sprite');
        
    //Resize the array
    SetLength(s^.bitmaps, Length(s^.bitmaps) + 1);

    //Add the values to the array
    s^.bitmaps[High(s^.bitmaps)] := bitmapToAdd;

    result := High(s^.bitmaps);
  end;

  /// Returns the current width of the sprite.
  ///
  /// @param sprite:     The sprite to get the width of
  /// @returns           The width of the sprite's current frame
  function SpriteWidth(s: Sprite): LongInt;
  begin
    if s = nil then raise Exception.Create('No sprite supplied');
  
    result := s^.width;
  end;

  /// Returns the current height of the sprite.
  ///
  /// @param sprite:     The sprite to get the height of
  /// @returns           The height of the sprite's current frame
  function SpriteHeight(s: Sprite): LongInt;
  begin
    if s = nil then raise Exception.Create('No sprite supplied');
  
    result := s^.height;
  end;

  procedure ReplayAnimation(s : Sprite);
  begin
    if s = nil then raise Exception.Create('No sprite supplied');

    s^.currentFrame := 0;
    s^.hasEnded := false;
    s^.reverse := false;
  end;

  procedure CycleFrame(s: Sprite);
    procedure EndAnimation(frame: LongInt);
    begin
      s^.currentFrame := frame;
      s^.hasEnded := true;
    end;
    procedure SetAnimation(frame: LongInt; reverse: Boolean);
    begin
      s^.currentFrame := frame;
      s^.reverse := reverse;
    end;
  begin
    if (s^.currentFrame > High(s^.framesPerCell)) then
    begin
      if (s^.endingAction = ReverseLoop) or (s^.endingAction = ReverseOnce) then
        SetAnimation(s^.currentFrame - 1, true)
      else if s^.endingAction = Loop then s^.currentFrame := 0
      else if s^.endingAction = Stop then EndAnimation(High(s^.framesPerCell));
    end
    else if (s^.currentFrame < Low(s^.framesPerCell)) then
    begin
      if s^.endingAction = ReverseOnce then EndAnimation(0)
      else SetAnimation(1, false);
    end;    
  end;

  procedure MoveToNextFrame(s: Sprite);
  var
    i, sum: LongInt;
    frameChange: LongInt;
  begin
    //Check that they are all + or 0, at at least one is +
    sum := 0;
    for i := Low(s^.framesPerCell) to High(s^.framesPerCell) do
    begin
      if s^.framesPerCell[i] < 0 then raise Exception.Create('Frames per cell must be 0 or positive');
      sum := sum + s^.framesPerCell[i];
    end;

    if sum = 0 then raise Exception.Create('Frames per cell cannot all be zero');
  
    //Reset the frame count.
    s^.frameCount := 0;
  
    if s^.reverse then frameChange := -1
    else frameChange := +1;
    
    s^.currentFrame := s^.currentFrame + frameChange;
  
    if (s^.currentFrame > High(s^.framesPerCell)) or
       (s^.currentFrame < Low(s^.framesPerCell)) then
    begin
      CycleFrame(s);
    end;
  end;

  procedure UpdateSpriteAnimation(s: Sprite); overload;
  begin
    UpdateSpriteAnimation(s, 1.0);
  end;

  /// Update the frame position
  ///
  /// @param s:  The sprite to be processed
  /// @param pct: Percentage to update
  ///
  /// Side Effects:
  /// - Process the frame position depending on the sprite's setting
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  begin
    if s = nil then raise Exception.Create('No sprite supplied');
    if s^.hasEnded then exit;
    if s^.spriteKind = StaticSprite then exit;
      
    s^.frameCount := s^.frameCount + pct;
  
    // If we are at the end of the current frame... need to move to the next frame
    if s^.frameCount >= s^.framesPerCell[s^.currentFrame] then
    begin
      MoveToNextFrame(s);

      while s^.framesPerCell[s^.currentFrame] = 0 do
      begin
        MoveToNextFrame(s);
      end;

      if s^.spriteKind = AnimArraySprite then
      begin
        s^.width := s^.bitmaps[s^.currentFrame]^.width;
        s^.height := s^.bitmaps[s^.currentFrame]^.height;
      end;
    end;
  end;

  procedure UpdateSprite(s: Sprite); overload;
  begin
    UpdateSprite(s, 1.0);
  end;

  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  begin
    MoveSprite(s, pct);
    UpdateSpriteAnimation(s, pct);
  end;

  /// Draws a sprite to the screen, without using a view port.
  ///
  /// @param s:     The sprite to be drawn
  ///
  /// Side Effects:
  /// - The sprite is drawn to the screen, if within screen area
  procedure DrawSprite(s: Sprite); overload;
  begin
    DrawSprite(s, 0, 0);
  end;

  procedure DrawSprite(s : Sprite; const position: Point2D); overload;
  begin
    DrawSprite(s, Round(position.x), Round(position.y));
  end;

  procedure DrawSprite(s: Sprite; xOffset, yOffset: LongInt); overload;
  var
    srcX, srcY: LongInt;
  begin
    if not Assigned(s) then raise Exception.Create('No sprite supplied');

    if (s^.rotation <> 0) or (s^.zoom <> 1) then
    begin
      UpdateSpriteBuffers(s);
      DrawBitmap(s^.bufferBmp, s^.x + xOffset, s^.y + yOffset);
    end
    else
    begin
      if s^.spriteKind <> AnimMultiSprite then
      begin
        DrawBitmap(s^.bitmaps[s^.currentFrame], s^.x + xOffset, s^.y + yOffset);
      end
      else
      begin
        with s^ do
        begin
          srcX := (currentFrame mod cols) * width;
          srcY := (currentFrame - (currentFrame mod cols)) div cols * height;
        end;
    
        DrawBitmapPart(s^.bitmaps[0], srcX, srcY, 
                     s^.width, s^.height,
                     s^.x + xOffset, s^.y + yOffset);
      end;
    end;
  end;

    /// Determines if a sprite is off the screen.
  ///
  /// @param s:     The sprite to check the position of
  /// @returns          True if the sprite is off the screen
  function IsSpriteOffscreen(s : Sprite): Boolean;
  begin
    if s = nil then raise Exception.Create('No sprite supplied');
  
    if sgCamera.ToScreenX(s^.x) >= ScreenWidth() then result := true
    else if sgCamera.ToScreenX(s^.x) + SpriteWidth(s) < 0 then result := true
    else if sgCamera.ToScreenY(s^.y) >= ScreenHeight() then result := true
    else if sgCamera.ToScreenY(s^.y) + SpriteHeight(s) < 0 then result := true
    else result := false;
  end;

  procedure MoveSprite(s : Sprite; const movementVector : Vector); overload;
  begin
    MoveSprite(s, movementVector, 1.0);
  end;

  /// Moves a sprite based on information in a movement vector.
  ///
  /// @param s:     The sprite to move
  /// @param movementVector:   The vector containing the movement details
  procedure MoveSprite(s : Sprite; const movementVector : Vector; pct: Single); overload;
  var
    mvmt: Vector;
    trans: Matrix2D;
  begin
    if not Assigned(s) then raise Exception.Create('No sprite supplied');
  
    if s^.rotation <> 0 then
    begin
      trans := RotationMatrix(-s^.rotation);
      mvmt := MatrixMultiply(trans, movementVector);
    end
    else  mvmt := movementVector;
  
    s^.x := s^.x + (pct * mvmt.x);
    s^.y := s^.y + (pct * mvmt.y);
  end;

  /// Moves a sprite to a given x,y location.
  ///
  /// @param s:     the sprite being moved
  /// @param x, y:             the new location of the sprite
  ///
  /// Side Effects:
  /// - Moves the sprite, changing its x and y
  procedure MoveSpriteTo(s : Sprite; x,y : LongInt);
  begin
    if s = nil then raise Exception.Create('No sprite supplied');
  
    s^.x := x;
    s^.y := y;
  end;

  procedure MoveSprite(s: Sprite); overload;
  begin
    MoveSprite(s, 1.0);
  end;  

  procedure MoveSprite(s: Sprite; pct: Single); overload;
  begin
    MoveSprite(s, s^.movement, pct);
  end;

  function SpriteOnScreenAt(s: Sprite; x, y: LongInt): Boolean; overload;
  var
    bmp: Bitmap;
    offX1, offY1: LongInt;
    wx, wy: Single;
  begin
    if s = nil then raise Exception.Create('The specified sprite is nil');
  
    wx := ToWorldX(x);
    wy := ToWorldY(y);

    if wy > s^.y + SpriteHeight(s) then result := false
    else if wy < s^.y then result := false
    else if wx > s^.x + SpriteWidth(s) then result := false
    else if wx < s^.x then result := false
    else if not s^.usePixelCollision then result := true
    else
    begin
      if s^.spriteKind = AnimMultiSprite then
      begin
        offX1 := (s^.currentFrame mod s^.cols) * s^.width;
        offY1 := (s^.currentFrame - (s^.currentFrame mod s^.cols)) div s^.cols * s^.height;
        bmp := s^.bitmaps[0];
      end
      else
      begin
        bmp := s^.bitmaps[s^.currentFrame];
        offX1 := 0;
        offY1 := 0;
      end;
      result := IsPixelDrawnAtPoint(bmp, Round(wx - s^.x + offX1), Round(wy - s^.y + offY1));
    end;
  end;

  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteOnScreenAt(s, Round(pt.x), Round(pt.y));
  end;

  function SpriteMovement(s: Sprite): Vector;
  begin
    result := s^.Movement;
  end;

  procedure SetSpriteMovement(s: Sprite; const value: Vector);
  begin
    s^.Movement := value;
  end;
  
  function SpriteFramesPerCell(s: Sprite): LongIntArray;
  begin
    result := s^.framesPerCell;
  end;
  
  procedure SetSpriteFramesPerCell(s: Sprite; const arr: LongIntArray);
  var
    i: Integer;
  begin
    for i := 0 to Min(High(s^.framesPerCell), High(arr)) do
    begin
      s^.framesPerCell[i] := arr[i];
    end;
  end;
  
  function SpriteCellCount(s: Sprite): LongInt;
  begin
    result := Length(s^.framesPerCell);
  end;

end.