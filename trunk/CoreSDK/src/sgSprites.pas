//=============================================================================
// sgSprites.pas
//=============================================================================
//
// Create and manage sprites in SwinGame.
//
// Change History:
//
// Version 3.0:
// - 2009-12-20: Andrew : Added code to manage sprite layers (show, hide, reorder, etc)
// - 2009-12-18: Andrew : Moved to new sprite format.
// - 2009-11-10: Andrew : Changed sn and csn tags
// - 2009-10-16: Andrew : Called the free notifier to ensure Sprites are freed
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
  uses sgTypes;
//=============================================================================

  /// Returns a `Vector` that is the difference in the position of two sprites
  /// (``s1`` and ``s2``).
  ///
  /// @lib
  /// @sn vectorFromSprite:%s toSprite:%s
  ///
  /// @class Sprite
  /// @method VectorTo
  /// @csn vectorToSprite:%s
  function VectorFromTo(s1, s2: Sprite): Vector;

  /// Returns a `Vector` that is the difference in location from the center of
  /// the sprite ``s`` to the point ``pt``.
  ///
  /// @lib
  /// @sn vectorFromCenterOfSprite:%s toPoint:%s
  ///
  /// @class Sprite
  /// @overload VectorTo VectorToPoint
  /// @csn vectorToPoint:%s
  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;


  //---------------------------------------------------------------------------
  // Sprite routines
  //---------------------------------------------------------------------------
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer have name 'layer1'.
  /// 
  /// @lib CreateBasicSprite
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s
  function CreateSprite(layer: Bitmap): Sprite; overload;
  
  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;
  
  function CreateSprite(layer: Bitmap; ani: AnimationTemplate): Sprite; overload;
  
  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationTemplate): Sprite; overload;
  
  function CreateSprite(const layers: BitmapArray): Sprite; overload;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  
  function CreateSprite(const layers: BitmapArray; ani: AnimationTemplate): Sprite; overload;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationTemplate): Sprite; overload;
  
  /// @lib
  /// @class Sprite
  /// @dispose
  procedure FreeSprite(var s : Sprite);
  
  
  //---------------------------------------------------------------------------
  // Layer code
  //---------------------------------------------------------------------------
  
  /// Adds a new layer to the sprite.
  /// 
  /// @lib
  /// @sn addTo:%s newLayer:%s named:%s
  ///
  /// @class Sprite
  /// @method AddLayer
  /// @csn addLayer:%s named:%s
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): LongInt;
  
  function SpriteLayer(s: Sprite; name: String): Bitmap;
  function SpriteLayer(s: Sprite; idx: LongInt): Bitmap;
  
  function SpriteShowLayer(s: Sprite; name: String): LongInt;
  function SpriteShowLayer(s: Sprite; id: LongInt): LongInt;
  
  procedure SpriteHideLayer(s: Sprite; name: String);
  procedure SpriteHideLayer(s: Sprite; id: LongInt);
  
  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): LongInt;
  function SpriteVisibleIndexOfLayer(s: Sprite; id: LongInt): LongInt;
  
  function SpriteLayerCount(s: Sprite): LongInt;
  function SpriteVisibleLayerCount(s: Sprite): LongInt;
  
  function SpriteVisibleLayerIds(s: Sprite) : LongIntArray;
  function SpriteLayers(s: Sprite): BitmapArray;
  
  function SpriteVisibleLayer(s: Sprite; idx: LongInt): LongInt;
  
  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: LongInt);
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: LongInt);
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: LongInt);
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: LongInt);
  
  //---------------------------------------------------------------------------
  // Animation code
  //---------------------------------------------------------------------------
  
  /// Restart the sprite's current animation, this will play a sound if the
  /// first cell of the animation is associated with a sound effect.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @method ReplayAnimation
  procedure SpriteReplayAnimation(s : Sprite);
  
  /// Restart the sprite's current animation, this will play a sound if
  /// withSound is true and the first cell of the animation is associated with a sound effect.
  /// 
  /// @lib ReplayAnimationWithSound
  /// @sn replayAnimation:%s withSound:%s
  /// 
  /// @class Sprite
  /// @overload ReplayAnimation ReplayAnimationWithSound
  /// @csn replayAnimationWithSound:%s
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean);
  
  /// Start playing an animation from the sprite's animation template.
  /// This will play a sound effect if the first cell of the animation
  /// has a sound.
  /// 
  /// @lib 
  /// 
  /// @class Sprite
  /// @method StartAnimation
  procedure SpriteStartAnimation(s: Sprite; named: String);
  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean);
  procedure SpriteStartAnimation(s: Sprite; idx: LongInt);
  procedure SpriteStartAnimation(s: Sprite; idx: LongInt; withSound: Boolean);
  
  /// @lib UpdateSpritePct(s, 1.0)
  /// @uname UpdateSprite
  ///
  /// @class Sprite
  /// @method Update
  procedure UpdateSprite(s: Sprite); overload;
  procedure UpdateSprite(s: Sprite; withSound:Boolean); overload;
    
  /// @lib UpdateSpritePct
  /// @class Sprite
  /// @overload Update UpdatePct
  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;
    
  /// @lib UpdateSpriteAnimationPct(s, 1.0)
  /// @uname UpdateSpriteAnimation
  /// @class Sprite
  /// @method UpdateAnimation
  procedure UpdateSpriteAnimation(s: Sprite); overload;
  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;
  
  /// @lib UpdateSpriteAnimationPct
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationPct
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;
  
  
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
    
  /// @lib MoveSpriteVecPct(s, velocity, 1.0)
  /// @uname MoveSpriteVec
  /// @class Sprite
  /// @overload Move MoveVec
  procedure MoveSprite(s : Sprite; const velocity: Vector); overload;
    
  /// @lib MoveSpriteVecPct
  /// @class Sprite
  ///@overload Move MoveVecPct
  procedure MoveSprite(s : Sprite; const velocity: Vector; pct: Single); overload;

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
  
  /// @lib
  /// @class Sprite
  /// @method IsOffscreen
  function IsSpriteOffscreen(s : Sprite): Boolean;
  
  
  
  //---------------------------------------------------------------------------
  // Sprite Width and Heigth - CenterPoint
  //---------------------------------------------------------------------------
  
  /// The current Height of the sprite (aligned to the Y axis).
  ///
  /// @lib
  /// @class Sprite
  /// @getter Height
  function SpriteHeight(s: Sprite): LongInt;
  
  /// The current Width of the sprite (aligned to the X axis).
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Width
  function SpriteWidth(s: Sprite): LongInt;
  
  /// Returns the center point of the passed in Sprite. This uses the Sprite's 
  /// Position, Width and Height.
  ///
  /// @lib CenterPoint
  ///
  /// @class Sprite
  /// @getter CenterPoint
  function CenterPoint(s: Sprite): Point2D; overload;
  
  //---------------------------------------------------------------------------
  // Sprite velocity
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @getter Velocity
  function SpriteVelocity(s: Sprite): Vector;

  /// @lib
  /// @class Sprite
  /// @setter Velocity
  procedure SpriteSetVelocity(s: Sprite; const value: Vector);

  //---------------------------------------------------------------------------
  // Sprite CellCount
  //---------------------------------------------------------------------------
  
  /// Returns a rectangle of the current cell within the Sprite's image.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter CurrentCellRectangle
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle;
  
  /// Returns the rectangle representing the location of the Sprite on the
  /// screen.
  ///
  /// @lib
  ///
  /// @class
  /// @getter ScreenRectangle
  function SpriteScreenRectangle(s: Sprite): Rectangle;
  
  //---------------------------------------------------------------------------
  // Sprite X,Y
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @setter X
  procedure SpriteSetX(s: Sprite; value: Single);
    
  /// @lib
  /// @class Sprite
  /// @getter X
  function SpriteX(s: Sprite): Single;
  
  /// @lib
  /// @class Sprite
  /// @setter Y
  procedure SpriteSetY(s: Sprite; value: Single);
    
  /// @lib
  /// @class Sprite
  /// @getter Y
  function SpriteY(s: Sprite): Single;
  
  //---------------------------------------------------------------------------
  // Sprite position
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @getter Position
  function SpritePosition(s: Sprite): Point2D;

  /// @lib
  /// @class Sprite
  /// @setter Position
  procedure SpriteSetPosition(s: Sprite; const value: Point2D);
  
  //---------------------------------------------------------------------------
  // Sprite DX,DY
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Sprite
  /// @setter DX
  procedure SpriteSetDX(s: Sprite; value: Single);
    
  /// @lib
  /// @class Sprite
  /// @getter DX
  function SpriteDX(s: Sprite): Single;
  
  /// @lib
  /// @class Sprite
  /// @setter DY
  procedure SpriteSetDY(s: Sprite; value: Single);
    
  /// @lib
  /// @class Sprite
  /// @getter DY
  function SpriteDY(s: Sprite): Single;
  
  //---------------------------------------------------------------------------
  // Sprite speed and heading
  //---------------------------------------------------------------------------
  
  /// Returns the current speed (distance travelled per update) of the Sprite.
  ///
  /// @lib
  /// @class Sprite
  /// @getter Speed
  function SpriteSpeed(s: Sprite): Single;
  
  /// Alters the speed of the Sprite without effecting the direction.
  ///
  /// @lib
  /// @class Sprite
  /// @setter Speed
  procedure SpriteSetSpeed(s: Sprite; value: Single);
  
  /// Returns the direction the Sprite is heading in degrees.
  ///
  /// @lib
  /// @class Sprite
  /// @getter Heading
  function SpriteHeading(s: Sprite): Single;
  
  /// Alters the direction the Sprite is heading without changing the speed.
  ///
  /// @lib
  /// @class Sprite
  /// @setter Heading
  procedure SpriteSetHeading(s: Sprite; value: Single);
  
  //---------------------------------------------------------------------------
  // Sprite Current Frame
  //---------------------------------------------------------------------------
  
  /// Returns the current animation cell for an Animated Sprite. The cell is
  /// updated when the sprite's animation data is updated.
  ///
  /// @lib
  /// @class Sprite
  /// @getter CurrentCell
  function SpriteCurrentCell(s: Sprite): LongInt;
  
  //---------------------------------------------------------------------------
  // Sprite collision details
  //---------------------------------------------------------------------------
  
  function SpriteCollisionBitmap(s: Sprite): Bitmap;
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap);
  
  function SpriteCollisionKind(s: Sprite): CollisionTestKind;
  
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind);
  
  /// Indicates if the sprites animation has ended.
  /// 
  /// @lib
  /// @class Sprite
  /// @getter AnimationHasEnded
  function SpriteAnimationHasEnded(s: Sprite): Boolean;
  
  //---------------------------------------------------------------------------
  // Sprite mass
  //---------------------------------------------------------------------------
  
  /// This indicates the mass of the Sprite for any of the collide methods from
  /// Physics. The mass of two colliding sprites will determine the relative
  /// velocitys after the collision.
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Mass
  function SpriteMass(s: Sprite): Single;
  
  /// Allows you to change the mass of a Sprite.
  ///
  /// @lib
  /// @class Sprite
  /// @setter Mass
  procedure SpriteSetMass(s: Sprite; value: Single);
  
  //---------------------------------------------------------------------------
  // Sprite rotation
  //---------------------------------------------------------------------------
  
  /// This indicates the angle of rotation of the Sprite. This will rotate any 
  /// images of the sprite before drawing, which can be very slow. Avoid using
  /// this method with bitmap based Sprites where possible.
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Rotation
  function SpriteRotation(s: Sprite): Single;

  /// Allows you to change the rotation of a Sprite.
  ///
  /// @lib
  /// @class Sprite
  /// @setter Rotation
  procedure SpriteSetRotation(s: Sprite; value: Single);
  
  //---------------------------------------------------------------------------
  // Sprite scale
  //---------------------------------------------------------------------------
  
  /// This indicates the scale (0 to 1.0) of the Sprite. This will scale any 
  /// images of the sprite before drawing, which can be very slow. Avoid using
  /// this method with bitmap based Sprites where possible.
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Scale
  function SpriteScale(s: Sprite): Single;

  /// Allows you to change the scale of a Sprite.
  ///
  /// @lib
  /// @class Sprite
  /// @setter Scale
  procedure SpriteSetScale(s: Sprite; value: Single);
  

//=============================================================================
implementation
  uses
    Classes, SysUtils, Math, // System
    sgNamedIndexCollection, //libsrc
    sgAnimations, sgGraphics, sgGeometry, sgCore, sgCamera, sgShared, sgResources, sgImages; //SwinGame
//=============================================================================

  
  const
    MASS_IDX      = 0;  // The index of the sprite's mass value
    ROTATION_IDX  = 1;  // The index of the sprite's rotation value
    SCALE_IDX     = 2;  // The index of the sprite's scale value
  
  
  function VectorFromTo(s1, s2: Sprite): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s1), CenterPoint(s2));
  end;

  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s), pt);
  end;
  
  function CreateSprite(layer: Bitmap): Sprite; overload;
  begin
    result := CreateSprite(layer, 'layer0', nil);
  end;
  
  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;
  begin
    result := CreateSprite(layer, layerName, nil);
  end;
  
  function CreateSprite(layer: Bitmap; ani: AnimationTemplate): Sprite; overload;
  begin
    result := CreateSprite(layer, 'layer0', ani);
  end;
  
  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationTemplate): Sprite; overload;
  var
    layerNames: StringArray;
    layers: BitmapArray;
  begin
    SetLength(layerNames, 1);
    SetLength(layers, 1);
    
    layerNames[0] := layerName;
    layers[0] := layer;
    
    result := CreateSprite(layers, layerNames, ani);
  end;
  
  function CreateSprite(const layers: BitmapArray): Sprite; overload;
  begin
    result := CreateSprite(layers, AnimationTemplate(nil));
  end;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  begin
    result := CreateSprite(layers, layerNames, nil);
  end;
  
  function CreateSprite(const layers: BitmapArray; ani: AnimationTemplate): Sprite; overload;
  var
    layerNames: StringArray;
    i: Integer;
  begin
    SetLength(layerNames, Length(layers));
    for i := 0 to Length(layers) do
    begin
      layerNames[i] := 'layer' + IntToStr(i);
    end;
    
    result := CreateSprite(layers, layerNames, ani);
  end;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationTemplate): Sprite; overload;
  var
    i, count: Integer;
  begin
    result := nil; 
    count := Length(layers);
    
    if count <> Length(layerNames) then begin RaiseException('The number of layers and layer names do not match.'); exit; end;
    if count = 0 then begin exit; end;
      
    //Allocate the space for the sprite
    New(result);
    
    //Set lengths of the layer arrays
    SetLength(result^.layers, count);
    
    for i := 0 to High(result^.layers) do
    begin
      result^.layers[i] := layers[i];
      
      // Make sure that this image can be used interchangably with the other layers of the
      // sprite.
      if not BitmapsInterchangable(result^.layers[i], result^.layers[0]) then
      begin
        Dispose(result);
        result := nil;
        RaiseException('Layer ' + IntToStr(i) + ' is not the interchangable with the other layers of this sprite.');
        exit;
      end;
    end;
    
    // Setup the layer name <-> id mapping
    InitNamedIndexCollection(result^.layerIds, layerNames);
    
    // Set the first layer as visible.
    SetLength(result^.visibleLayers, 1);
    result^.visibleLayers[0] := 0;                //The first layer (at idx 0) is drawn
    
    // Setup the values
    SetLength(result^.values, 3);
    InitNamedIndexCollection(result^.valueIds);
    
    AddName(result^.valueIds, 'mass');            //idx 0 = mass, default to 1
    result^.values[MASS_IDX]      := 1;
    
    AddName(result^.valueIds, 'rotation');        //idx 1 = rotation, default to 0
    result^.values[ROTATION_IDX]  := 0;
    
    AddName(result^.valueIds, 'scale');           //idx 2 = mass, default to 1
    result^.values[SCALE_IDX]     := 1;
    
    // Position the sprite
    result^.position              := PointAt(0,0);
    
    // Initialise sprite movement
    result^.velocity              := VectorTo(0,0);
    
    // Setup animation detials
    result^.animationTemplate     := ani;
    result^.animationData         := nil;
    
    // Setup collision details
    result^.collisionKind         := PixelCollisions;
    result^.collisionBitmap       := result^.layers[0];
  end;
  
  // //
  // // Update the buffered image for rotation and scaling of a bitmap based sprite.
  // //
  // procedure UpdateSpriteBuffers(s: Sprite);
  // var
  //   dest: Bitmap; //temporary surface
  //   srcX, srcY: LongInt; //for image parts
  // begin
  //   if (s^.rotation = s^.bufferedRotation) and (s^.scale = s^.bufferedScale) then exit;
  //   if (s^.bufferBmp <> nil) then FreeBitmap(s^.bufferBmp);
  //   if (s^.rotation = 0) and (s^.scale = 1) then exit; //no need to transform
  // 
  //   //Draw non-transformed bitmap onto temp surface
  //   dest := CreateBitmap(s^.width, s^.height);
  // 
  //   if s^.spriteKind <> AnimMultiSprite then
  //     DrawBitmap(dest, s^.bitmaps[s^.currentCell], 0, 0)
  //   else
  //   begin
  //     with s^ do
  //     begin
  //       srcX := (currentCell mod cols) * width;
  //       srcY := (currentCell - (currentCell mod cols)) div cols * height;
  //     end;
  // 
  //     MakeOpaque(s^.bitmaps[0]);
  //     DrawBitmapPart(dest, s^.bitmaps[0], srcX, srcY, s^.width, s^.height, 0, 0);
  //     MakeTransparent(s^.bitmaps[0]);
  //   end;
  // 
  //   s^.bufferBmp := RotateScaleBitmap(dest, s^.rotation, s^.scale);
  // 
  //   FreeBitmap(dest);
  // end;

  procedure FreeSprite(var s : Sprite);
  begin
    if Assigned(s) then
    begin
      // Free arrays
      SetLength(s^.layers, 0);
      SetLength(s^.visibleLayers, 0);
      SetLength(s^.values, 0);
      
      // Free the name <-> id maps
      FreeNamedIndexCollection(s^.layerIds);
      FreeNamedIndexCollection(s^.valueIds);
      
      // Nil pointers
      s^.animationData := nil;
      s^.animationTemplate := nil;
      s^.collisionBitmap := nil;
      
      //Free buffered rotation image
      // if s^.bufferBmp <> nil then FreeBitmap(s^.bufferBmp);
      // s^.bufferBmp := nil;
    
      //Dispose sprite
      Dispose(s);
      CallFreeNotifier(s);
      s := nil;
    end;
  end;
  
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): LongInt;
  begin
    if newLayer = nil then begin RaiseException('Cannot add non-existing bitmap to Sprite'); exit; end;
    if s = nil then begin RaiseException('No sprite to add to'); exit; end;
    
    result := AddName(s^.layerIds, layerName);
    if (result < 0) or (result > Length(s^.layers)) then begin RaiseException('Error adding layer ' + layerName); exit; end;
    
    //Resize the array
    SetLength(s^.layers, Length(s^.layers) + 1);
    
    //Add the values to the array
    s^.layers[result] := newLayer;
  end;

  /// Returns the current width of the sprite.
  ///
  /// @param sprite:     The sprite to get the width of
  /// @returns           The width of the sprite's current frame
  function SpriteWidth(s: Sprite): LongInt;
  begin
    if s = nil then
      result := 0
    else
      result := s^.layers[0]^.cellW;
  end;

  /// Returns the current height of the sprite.
  ///
  /// @param sprite:     The sprite to get the height of
  /// @returns           The height of the sprite's current frame
  function SpriteHeight(s: Sprite): LongInt;
  begin
    if s = nil then
      result := 0
    else
      result := s^.layers[0]^.cellH;
  end;

  procedure SpriteReplayAnimation(s: Sprite);
  begin
    SpriteReplayAnimation(s, true);
  end;
  
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean);
  begin
    if s = nil then exit;
    
    RestartAnimation(s^.animationData, withSound);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; named: String);
  begin
    SpriteStartAnimation(s, named, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean);
  begin
    SpriteStartAnimation(s, AnimationIndex(s^.animationTemplate, named), withSound);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: LongInt);
  begin
    SpriteStartAnimation(s, idx, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: LongInt; withSound: Boolean);
  begin
    
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite); overload;
  begin
    UpdateSpriteAnimation(s, 1.0, true);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;
  begin
    UpdateSpriteAnimation(s, 1.0, withSound);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  begin
    UpdateSpriteAnimation(s, pct, true);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;
  begin
    if s = nil then exit;
    
    UpdateAnimation(s^.animationData, pct, withSound);
  end;

  procedure UpdateSprite(s: Sprite); overload;
  begin
    UpdateSprite(s, 1.0, true);
  end;
  
  procedure UpdateSprite(s: Sprite; withSound: Boolean); overload;
  begin
    UpdateSprite(s, 1.0, withSound);
  end;
  
  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  begin
    UpdateSprite(s, pct, true);
  end;
  
  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;
  begin
    MoveSprite(s, pct);
    UpdateSpriteAnimation(s, pct);
  end;
  
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
    i: LongInt;
  begin
    if not Assigned(s) then begin RaiseException('No sprite supplied'); exit; end;
    
    for i := 0 to High(s^.visibleLayers) do
    begin
      DrawCell(SpriteLayer(s, s^.visibleLayers[i]), SpriteCurrentCell(s), Round(s^.position.x + xOffset), Round(s^.position.y + yOffset));
    end;
  end;

  function IsSpriteOffscreen(s : Sprite): Boolean;
  begin
    if s = nil then 
      result := false
    else
      result := not OnScreen(RectangleFrom(s));
  end;

  procedure MoveSprite(s : Sprite; const velocity : Vector); overload;
  begin
    MoveSprite(s, velocity, 1.0);
  end;

  procedure MoveSprite(s : Sprite; const velocity : Vector; pct: Single); overload;
  var
    mvmt: Vector;
  //   trans: Matrix2D;
  begin
    if not Assigned(s) then begin RaiseException('No sprite supplied'); exit; end;
    
    // if s^.rotation <> 0 then
    // begin
    //   trans := RotationMatrix(-s^.rotation);
    //   mvmt := MatrixMultiply(trans, velocity);
    // end
    // else  
    mvmt := velocity;
       
    s^.position.x := s^.position.x + (pct * mvmt.x);
    s^.position.y := s^.position.y + (pct * mvmt.y);
  end;

  procedure MoveSpriteTo(s : Sprite; x,y : LongInt);
  begin
    if s = nil then begin RaiseException('No sprite supplied'); exit; end;
  
    s^.position.x := x;
    s^.position.y := y;
  end;

  procedure MoveSprite(s: Sprite); overload;
  begin
    MoveSprite(s, 1.0);
  end;  

  procedure MoveSprite(s: Sprite; pct: Single); overload;
  begin
    MoveSprite(s, s^.velocity, pct);
  end;

  function SpriteOnScreenAt(s: Sprite; x, y: LongInt): Boolean; overload;
  var
    cellRect: Rectangle;
    wx, wy: Single;
  begin
    result := false;
    
    if not assigned(s) then exit;
    
    wx := ToWorldX(x);
    wy := ToWorldY(y);
    
    if wy > s^.position.y + SpriteHeight(s) then      result := false
    else if wy < s^.position.y then                   result := false
    else if wx > s^.position.x + SpriteWidth(s) then  result := false
    else if wx < s^.position.x then                   result := false
    else if s^.collisionKind = AABBCollisions then    result := true
    else
    begin
      cellRect := SpriteCurrentCellRectangle(s);
      result := PixelDrawnAtPoint(s^.collisionBitmap, Round(wx - s^.position.x + cellRect.x), Round(wy - s^.position.y + cellRect.y));
    end;
  end;

  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteOnScreenAt(s, Round(pt.x), Round(pt.y));
  end;

  function SpriteVelocity(s: Sprite): Vector;
  begin
    result := s^.velocity;
  end;

  procedure SpriteSetVelocity(s: Sprite; const value: Vector);
  begin
    s^.velocity := value;
  end;
  
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle;
  begin
    if (not Assigned(s)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := RectangleOfCell(s^.layers[0], AnimationCurrentCell(s^.animationData));
  end;
  
  function SpriteScreenRectangle(s: Sprite): Rectangle;
  begin
    if (not Assigned(s)) or (not Assigned(s^.animationData)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := ToScreen(RectangleFrom(s));
  end;
  
  procedure SpriteSetX(s: Sprite; value: Single);
  begin
    s^.position.x := value;
  end;
    
  function SpriteX(s: Sprite): Single;
  begin
    result := s^.position.x;
  end;
  
  procedure SpriteSetY(s: Sprite; value: Single);
  begin
    s^.position.y := value;
  end;
    
  function SpriteY(s: Sprite): Single;
  begin
    result := s^.position.y;
  end;
  
  function SpriteLayer(s: Sprite; name: String): Bitmap;
  begin
    if not assigned(s) then result := nil
    else result := SpriteLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayer(s: Sprite; idx: LongInt): Bitmap;
  begin
    if not assigned(s) then result := nil
    else if (idx < 0) or (idx > High(s^.layers)) then begin result := nil; RaiseException('Sprite layer index out of range - ' + IntToStr(idx)); exit; end
    else result := s^.layers[idx];
  end;
  
  function SpriteShowLayer(s: Sprite; name: String): LongInt;
  begin
    if not assigned(s) then result := -1
    else result := SpriteShowLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteShowLayer(s: Sprite; id: LongInt): LongInt;
  begin
    if not assigned(s) then result := -1
    else
    begin
      //Scan for the current ID
      result := SpriteVisibleIndexOfLayer(s, id);
      if result >= 0 then exit;
      
      //Extend layers and add index
      SetLength(s^.visibleLayers, Length(s^.visibleLayers) + 1);
      result := High(s^.visibleLayers);
      s^.visibleLayers[result] := id;
    end;
  end;
  
  procedure SpriteHideLayer(s: Sprite; name: String);
  begin
    if not assigned(s) then exit
    else SpriteHideLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  procedure SpriteHideLayer(s: Sprite; id: LongInt);
  var
    i, idx: LongInt;
  begin
    idx := SpriteVisibleIndexOfLayer(s, id);
    if idx < 0 then exit; // The layer is not shown
    
    //Shift all later layers back over removed layer
    for i := idx to High(s^.visibleLayers) - 1 do
    begin
      s^.visibleLayers[i] := s^.visibleLayers[i + 1];
    end;
    
    //Resize the array to remove element
    SetLength(s^.visibleLayers, Length(s^.visibleLayers) - 1);
  end;
  
  function SpriteLayerCount(s: Sprite): LongInt;
  begin
    if not assigned(s) then result := 0
    else result := Length(s^.layers);
  end;
  
  function SpriteVisibleLayerCount(s: Sprite): LongInt;
  begin
    if not assigned(s) then result := 0
    else result := Length(s^.visibleLayers);
  end;
  
  function SpriteVisibleLayerIds(s: Sprite) : LongIntArray;
  begin
    if not assigned(s) then result := nil
    else result := s^.visibleLayers;
  end;
  
  function SpriteLayers(s: Sprite): BitmapArray;
  begin
    if not assigned(s) then result := nil
    else result := s^.layers;
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): LongInt;
  begin
    if not assigned(s) then result := -1
    else result := SpriteVisibleIndexOfLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; id: LongInt): LongInt;
  var
    i: LongInt;
  begin
    result := -1;
    if not assigned(s) then exit
    else
    begin
      for i := 0 to High(s^.visibleLayers) do
      begin
        if s^.visibleLayers[i] = id then 
        begin
          result := i;
          exit;
        end;
      end;
    end;
  end;
  
  function SpriteVisibleLayer(s: Sprite; idx: LongInt): LongInt;
  begin
    result := -1;
    if not assigned(s) then exit
    else result := s^.visibleLayers[idx];
  end;
  
  procedure Swap(var val1, val2: LongInt);
  var
    temp: LongInt;
  begin
    temp := val1;
    val1 := val2;
    val2 := temp;
  end;
  
  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: LongInt);
  var
    i: LongInt;
  begin
    if not assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    // Bubble layer up
    for i := visibleLayer to High(s^.visibleLayers) - 1 do
    begin
      Swap(s^.visibleLayers[i], s^.visibleLayers[i + 1]);
    end;
  end;
  
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: LongInt);
  begin
    if not assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    Swap(s^.visibleLayers[visibleLayer], s^.visibleLayers[visibleLayer + 1]);
  end;
  
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: LongInt);
  begin
    if not assigned(s) then exit;
    //Check not first or lower
    if (visibleLayer < 1) or (visibleLayer >= Length(s^.visibleLayers)) then exit;
    
    Swap(s^.visibleLayers[visibleLayer], s^.visibleLayers[visibleLayer - 1]);
  end;
  
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: LongInt);
  var
    i: LongInt;
  begin
    if not assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    // Bubble layer down
    for i := visibleLayer downto 1 do
    begin
      Swap(s^.visibleLayers[i], s^.visibleLayers[i - 1]);
    end;
  end;

  //---------------------------------------------------------------------------
  // Sprite position
  //---------------------------------------------------------------------------
  
  function SpritePosition(s: Sprite): Point2D;
  begin
    result := s^.position;
  end;
  
  procedure SpriteSetPosition(s: Sprite; const value: Point2D);
  begin
    s^.position := value;
  end;
  
  //---------------------------------------------------------------------------
  // Sprite DX,DY
  //---------------------------------------------------------------------------
  
  procedure SpriteSetDX(s: Sprite; value: Single);
  begin
    s^.velocity.x := value;
  end;
  
  function SpriteDX(s: Sprite): Single;
  begin
    result := s^.velocity.x;
  end;
  
  procedure SpriteSetDY(s: Sprite; value: Single);
  begin
    s^.velocity.y := value;
  end;
  
  function SpriteDY(s: Sprite): Single;
  begin
    result := s^.velocity.y;
  end;
  
  //---------------------------------------------------------------------------
  // Sprite speed and heading
  //---------------------------------------------------------------------------
  
  function SpriteSpeed(s: Sprite): Single;
  begin
    result := VectorMagnitude(s^.velocity);
  end;
  
  procedure SpriteSetSpeed(s: Sprite; value: Single);
  begin
    s^.velocity := VectorMultiply(UnitVector(s^.velocity), value);
  end;
  
  function SpriteHeading(s: Sprite): Single;
  begin
    result := VectorAngle(s^.velocity);
  end;
  
  procedure SpriteSetHeading(s: Sprite; value: Single);
  begin
    s^.velocity := VectorFromAngle(value, VectorMagnitude(s^.velocity));
  end;
  
  //---------------------------------------------------------------------------
  // Sprite Current cell
  //---------------------------------------------------------------------------
  
  function SpriteCurrentCell(s: Sprite): LongInt;
  begin
    if not assigned(s) then result := -1
    else result := AnimationCurrentCell(s^.animationData);
  end;
  
  function SpriteAnimationHasEnded(s: Sprite): Boolean;
  begin
    if not assigned(s) then result := false
    else result := AnimationEnded(s^.animationData);
  end;
  
  //---------------------------------------------------------------------------
  // Sprite mass
  //---------------------------------------------------------------------------
  
  function SpriteMass(s: Sprite): Single;
  begin
    result := s^.values[MASS_IDX];
  end;
  
  procedure SpriteSetMass(s: Sprite; value: Single);
  begin
    s^.values[MASS_IDX] := value;
  end;
  
  //---------------------------------------------------------------------------
  // Sprite rotation
  //---------------------------------------------------------------------------
  
  function SpriteRotation(s: Sprite): Single;
  begin
    result := s^.values[ROTATION_IDX];
  end;
  
  procedure SpriteSetRotation(s: Sprite; value: Single);
  begin
    s^.values[ROTATION_IDX] := value;
  end;
  
  //---------------------------------------------------------------------------
  // Sprite scale
  //---------------------------------------------------------------------------
  
  function SpriteScale(s: Sprite): Single;
  begin
    result := s^.values[SCALE_IDX];
  end;
  
  procedure SpriteSetScale(s: Sprite; value: Single);
  begin
    s^.values[SCALE_IDX] := value;
  end;
  
  //---------------------------------------------------------------------------
  // Sprite center point
  //---------------------------------------------------------------------------
  
  function CenterPoint(s: Sprite): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}
    
    result.x := s^.position.x + SpriteWidth(s) / 2;
    result.y := s^.position.y + SpriteHeight(s) / 2;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}
  end;
  
  //---------------------------------------------------------------------------
  // Sprite collision details
  //---------------------------------------------------------------------------
  
  function SpriteCollisionKind(s: Sprite): CollisionTestKind;
  begin
    if not assigned(s) then result := AABBCollisions
    else result := s^.collisionKind;
  end;
  
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind);
  begin
    if assigned(s) then s^.collisionKind := value;
  end;
  
  function SpriteCollisionBitmap(s: Sprite): Bitmap;
  begin
    if not assigned(s) then result := nil
    else result := s^.collisionBitmap;
  end;
    
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap);
  begin
    if assigned(s) then s^.collisionBitmap := bmp;
  end;

//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;
end.