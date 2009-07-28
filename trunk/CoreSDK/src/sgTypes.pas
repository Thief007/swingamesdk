//=============================================================================
// sgTypes.pas
//=============================================================================
//
// The Types unit contains the data types used by SwinGames for shapes,
// Sprites, Bitmaps, Sounds, etc.
//
// Change History:
//
// Version 3.0:
// - 2009-07-13: Clinton: Renamed Event to MapEvent to MapTag
//                      : Renamed EventDetails to MapTagDetails
//                      : Renamed LayerData to MapLayerData
//                      : Renamed Tile to MapTile
//                      : Renamed CollisionData to MapCollisionData
// - 2009-07-06: Andrew : Changed movement to velocity and x,y to position for Sprite 
// - 2009-07-03: Andrew : Added sameas attribute to allow implicit casts in C#
// - 2009-07-02: Andrew : Formatting, added @via_pointer for types accessed via a pointer
//                      : Added fields to meta comments for Vector
// - 2009-06-29: Andrew : Added Circle
// -                    : Started Polygon (removed for version 3)
// - 2009-06-20: Andrew : Created types unit.
//=============================================================================

/// @header sgTypes
unit sgTypes;

//=============================================================================
interface
//=============================================================================

  uses SDL_Mixer, SDL, SDL_Image, SDL_TTF;
  
  type


    /// @type LongIntArray
    /// @array_wrapper
    /// @field data: array of LongInt
    LongIntArray = array of LongInt;

    /// @type LongIntPtr
    /// @pointer_wrapper
    /// @field pointer: ^LongInt
    LongIntPtr = ^LongInt;
    

    /// A Point2D represents an location in Cartesian coordinates (x,y).
    ///
    /// @struct Point2D
    /// @sameas Vector
    Point2D = record
      x, y: Single;
    end;

    /// Vectors represent a direction and distance, stored as x,y components.
    ///
    /// @struct Vector
    /// @field x: Single
    /// @field y: Single
    /// @sameas Point2D
    Vector = Point2D;

    /// @type Point2DPtr
    /// @pointer_wrapper
    /// @field pointer: ^Point2D
    Point2DPtr = ^Point2D;

    /// @type Point2DArray
    /// @array_wrapper
    /// @field data: array of Point2D
    ArrayOfPoint2D = Array of Point2D;

    // /// The kind of polygon.
    // ///
    // /// @enum PolygonKind
    // PolygonKind= (
    //     pkLine,
    //     pkStrip,
    //     pkFan,
    //     pkList
    //   );
    //
    // /// @struct Polygon
    // Polygon = record
    //   points: ArrayOfPoint2D;
    //   kind: PolygonKind;
    // end;

    /// @struct Rectangle
    Rectangle = record
      x, y: Single;
      width, height: LongInt;
    end;

    /// @struct Circle
    Circle = record
      center: Point2D;
      radius: LongInt;
    end;

    /// @struct LineSegment
    LineSegment = record
        startPoint: Point2D;
        endPoint: Point2D;
      end;

    /// @struct Triangle
    /// @fixed_array_wrapper
    /// @field data: array[0..2] of Point2D
    Triangle = array [0..2] of Point2D;

    /// @type LinesArray
    /// @array_wrapper
    /// @field data: LineSegmentPtr
    LinesArray = Array of LineSegment;

    /// @type LineSegmentPtr
    /// @pointer_wrapper
    LineSegmentPtr = ^LineSegment;

    /// The `SoundEffect` type is used to refer to sound effects that can be
    /// played by the SwinGame audio code. Sound effects are loaded with
    /// `LoadSoundEffect`, played using `PlaySoundEffect`, and must be
    /// released using `FreeMusic`.
    ///
    /// SwinGame will mix the audio from multiple sound effects, making it
    /// possible to play multiple SoundEffects, or even to play the one
    /// SoundEffect multiple times.
    ///
    /// You can check if a SoundEffect is currently playing using
    /// `IsSoundEffectPlaying`.
    ///
    /// To stop a SoundEffect playing use `StopSoundEffect`. This will stop all
    /// instances of this one sound effect from playing.
    ///
    /// @note Use `Music` for background music for your games.
    ///
    /// @class SoundEffect
    /// @pointer_wrapper
    /// @field pointer: pointer
    SoundEffect = PMix_Chunk;

    /// The SoundEffect type is used to refer to sound effects that can be
    /// played by the SwinGame audio code. Sound effects are loaded with
    /// `LoadSoundEffect`, played using `PlaySoundEffect`, and must be
    /// released using `FreeMusic`.
    ///
    /// SwinGame will mix the audio from multiple sound effects, making it
    /// possible to play multiple SoundEffects, or even to play the one
    /// SoundEffect multiple times.
    ///
    /// You can check if a SoundEffect is currently playing using
    /// `IsSoundEffectPlaying`.
    ///
    /// To stop a SoundEffect playing use `StopSoundEffect`. This will stop all
    /// instances of this one sound effect from playing.
    ///
    /// @note Use `SoundEffect` for the foreground sound effects of for your games.
    ///
    /// @class Music
    /// @pointer_wrapper
    /// @field pointer: pointer
    Music = PMix_Music;

    /// In SwinGame, Matrices can be used to combine together a number of
    /// operations that need to be performed on Vectors.
    ///
    /// @struct Matrix2D
    /// @fixed_array_wrapper
    /// @field data: array[0..2,0..2] of Single
    /// @sameas Array [0..2,0..2] of Single
    Matrix2D = Array [0..2,0..2] of Single;

    /// @type SinglePtr
    /// @pointer_wrapper
    /// @field pointer: ^Single
    SinglePtr = ^Single;

    /// The CollisionSide enumeration is used to indicate the side a collision
    /// has occurred on.
    ///
    /// @enum CollisionSide
    CollisionSide = (
      Top,
      Bottom,
      Left,
      Right,
      TopLeft,
      TopRight,
      BottomLeft,
      BottomRight,
      None
    );

    /// The color type is used within the SwinGameAPI to store color values.
    /// The color values are represented as 32bit RGBA values where R stores the
    /// color's red component, G stores the green component, B stores the blue
    /// component and A stores an alpha value representing the opacity (transparency)
    ///  of the of the color.
    ///
    /// @type Color
    /// @data_wrapper
    /// @field data: UInt32
    Color = UInt32;

    /// Bitmap data stores the data associated with a Bitmap. Each bitmap contains
    /// a pointer to the bitmap color information (surface), its width, height,
    /// and a mask storing the non-transparent pixels that is used for pixel level
    /// collision checking.
    ///
    /// @note Do not use BitmapData directly, use Bitmap.
    /// @struct BitmapData
    /// @via_pointer
    BitmapData = record
      surface: PSDL_Surface;
      width, height: LongInt;
      nonTransparentPixels: Array of Array of Boolean;
    end;

    /// The bitmap type is a pointer to a BitmapData. The BitmapData record
    /// contains the data used by the SwinGame API to represent
    /// bitmaps. You can create new bitmaps in memory for drawing operatings
    /// using the `CreateBitmap` function. This can then be optimised for drawing
    /// to the screen using the `OptimiseBitmap` routine. Also see the `DrawBitmap`
    /// routines.
    ///
    /// @class Bitmap
    /// @pointer_wrapper
    /// @field pointer: ^BitmapData
    Bitmap = ^BitmapData;

    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across different platforms
    ///
    /// @enum ResourceKind
    ResourceKind = (
      FontResource,
      BitmapResource,
      SoundResource,
      MusicResource,
      MapResource,
      PanelResource,
      OtherResource
    );

    /// The sprite kind is used to indicate the type for each Sprite. This controls
    /// the update behaviour of the Sprite. A StaticSprite will not animate at all and
    /// the update sprite only performs sprite velocity. An AnimArraySprite will animate
    /// using an array of bitmaps in addition to updating the Sprite's position, where as
    ///  a AnimMultiSprite will animate using a single bitmap which contains multiple
    /// frames. The sprite kind is determined when the `Sprite` is created. The
    /// `SpriteEndingAction` values are used in conjuncture with the `SpriteKind` to
    /// control the behaviour of the animation.
    ///
    /// @enum SpriteKind
    SpriteKind = (
      StaticSprite,
      AnimArraySprite,
      AnimMultiSprite
    );

    /// The sprite ending action is used to determine what action is peformed when a
    /// sprite gets to the end of its frames in its animation. Loop indicates that the
    /// sprite will restart the animation from the first frame resulting in the animation
    /// being looped repeatedly. ReverseLoop also loops the animation, but rather than
    /// returning to the first frame the ReverseLoop option indicates that the animation
    /// should play backwards until it gets to the start of the cells. With ReverseLoop the
    /// animation will play forward, then backward, then forward, etc. repeatedly. To
    /// reverse the animation without looping the ReverseOnce can be used. This plays the
    /// animation forward once, then back once, then stops. Finally, the Stop option means
    /// that the animation stops once it gets to the end of the framces. Once an animation
    /// stops sprite will be drawn as the last frame in the animation.
    ///
    /// @enum SpriteEndingAction
    SpriteEndingAction = (
      Loop,
      ReverseLoop,
      ReverseOnce,
      Stop
    );


    /// NOTE: Do not use SpriteData directly. Use Sprite.
    ///
    /// - bitmaps: The array of bitmaps related to the Sprite
    /// - spriteKind: Animation kind of this sprite
    /// - framesPerCell: Array of Integer that defines the frames per cell
    /// - xPod, yPos: The sprites location within the game world
    /// - width, height: The width and height of this sprite (used for multi)
    /// - cols, row: The number of cols and rows of this sprite (used for multi)
    /// - frameCount: Current frame count of this sprite
    /// - currentFrame: The current animation cell for the Sprite
    /// - usePixelCollision: A flag indicating if pixel collision sould be
    ///                      used, if false bounding collision is used.
    /// - endingAction: How this sprite acts when it finishes playing the animation
    /// - hasEnded: True if this sprite has stopped animating
    /// - reverse: True if this sprite's animation is reversing
    ///
    /// @struct SpriteData
    /// @via_pointer
    SpriteData = record
      bitmaps: Array of Bitmap;
      bufferBmp: Bitmap;
      spriteKind: SpriteKind;
      framesPerCell: LongIntArray; //Array of LongInt;
      position: Point2D;
      velocity: Vector;
      width: LongInt;
      height: LongInt;
      cols: LongInt;
      rows: LongInt;
      frameCount: Single;
      currentCell: LongInt;
      usePixelCollision: Boolean;
      endingAction: SpriteEndingAction;
      hasEnded: Boolean;
      reverse: Boolean;
      mass: Single;
      rotation: Single;
      scale: Single;

      bufferedRotation: Single;
      bufferedScale: Single;
    end;

    /// Sprites are used to represent Sprites drawn to the screen. Create a
    /// sprite using the CreateSprite function, and free it when complete with
    /// the FreeSprite function. The sprite contain a number of bitmaps used to
    /// store animations, or the like. Sprite drawing operations will draw the
    /// Sprite's current frame.
    ///
    /// @class Sprite
    /// @pointer_wrapper
    /// @field pointer: ^SpriteData
    Sprite = ^SpriteData;

    /// @struct TimerData
    /// @via_pointer
    TimerData = record
      startTicks: UInt32;
      pausedTicks: UInt32;
      paused: Boolean;
      started: Boolean;
    end;

    /// @class Timer
    /// @pointer_wrapper
    /// @field pointer: ^TimerData
    Timer = ^TimerData;

    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    /// DrawText and DrawTextLines routines.
    ///
    /// @class Font
    /// @pointer_wrapper
    /// @field pointer: pointer
    Font = PTTF_Font;

    /// Use font styles to set the style of a font. Setting the style is time
    /// consuming, so create alternative font variables for each different
    /// style you want to work with. Note that these values can be logical
    /// ORed together to combine styles, e.g. BoldFont or ItalicFont = both
    /// bold and italic.
    ///
    /// @enum FontStyle
    FontStyle = (
        NormalFont      = 0,
        BoldFont        = 1,
        ItalicFont      = 2,
        UnderlineFont   = 4
      );

    /// Use font alignment for certain drawing operations. With these
    /// operations you specify the area to draw in as well as the alignment
    /// within that area. See DrawTextLines.
    ///
    /// @enum FontAlignment
    FontAlignment = (
        AlignLeft   = 1,
        AlignCenter = 2,
        AlignRight  = 4
      );

    /// @type BitmapArray
    /// @array_wrapper
    /// @field data: array of Bitmap
    BitmapArray = array of Bitmap;

    /// @type BitmapPtr
    /// @pointer_wrapper
    /// @field pointer: ^Bitmap
    BitmapPtr = ^Bitmap;

    /// A mouse can have many different types of buttons. Most people know
    /// about the simple Left and Right buttons, but there is also a Middle
    /// button (sometimes part of a scoll wheel). Scroll wheel movement is also
    /// treated as mouse button "clicks" of either the wheel "up" or "down"
    /// buttons.
    ///
    /// @enum MouseButton
    MouseButton = (
      NoButton,
      LeftButton,
      MiddleButton,
      RightButton,
      WheelUpButton,
      WheelDownButton,
      MouseX1Button,
      MouseX2Button
    );

    /// @enum KeyCode
    KeyCode = (
      vk_Unknown = 0,
      vk_BACKSPACE = 8,
      vk_TAB = 9,
      vk_CLEAR = 12,
      vk_RETURN = 13,
      vk_PAUSE = 19,
      vk_ESCAPE = 27,
      vk_SPACE = 32,
      vk_EXCLAIM = 33,
      vk_QUOTEDBL = 34,
      vk_HASH = 35,
      vk_DOLLAR = 36,
      vk_AMPERSAND = 38,
      vk_QUOTE = 39,
      vk_LEFTPAREN = 40,
      vk_RIGHTPAREN = 41,
      vk_ASTERISK = 42,
      vk_PLUS = 43,
      vk_COMMA = 44,
      vk_MINUS = 45,
      vk_PERIOD = 46,
      vk_SLASH = 47,
      vk_0 = 48,
      vk_1 = 49,
      vk_2 = 50,
      vk_3 = 51,
      vk_4 = 52,
      vk_5 = 53,
      vk_6 = 54,
      vk_7 = 55,
      vk_8 = 56,
      vk_9 = 57,
      vk_COLON = 58,
      vk_SEMICOLON = 59,
      vk_LESS = 60,
      vk_EQUALS = 61,
      vk_GREATER = 62,
      vk_QUESTION = 63,
      vk_AT = 64,

      // Skip uppercase letters

      vk_LEFTBRACKET = 91,
      vk_BACKSLASH = 92,
      vk_RIGHTBRACKET = 93,
      vk_CARET = 94,
      vk_UNDERSCORE = 95,
      vk_BACKQUOTE = 96,
      vk_a = 97,
      vk_b = 98,
      vk_c = 99,
      vk_d = 100,
      vk_e = 101,
      vk_f = 102,
      vk_g = 103,
      vk_h = 104,
      vk_i = 105,
      vk_j = 106,
      vk_k = 107,
      vk_l = 108,
      vk_m = 109,
      vk_n = 110,
      vk_o = 111,
      vk_p = 112,
      vk_q = 113,
      vk_r = 114,
      vk_s = 115,
      vk_t = 116,
      vk_u = 117,
      vk_v = 118,
      vk_w = 119,
      vk_x = 120,
      vk_y = 121,
      vk_z = 122,
      vk_DELETE = 127,
      // End of ASCII mapped keysyms

      // International keyboard syms
      vk_WORLD_0 = 160, // 0xA0
      vk_WORLD_1 = 161,
      vk_WORLD_2 = 162,
      vk_WORLD_3 = 163,
      vk_WORLD_4 = 164,
      vk_WORLD_5 = 165,
      vk_WORLD_6 = 166,
      vk_WORLD_7 = 167,
      vk_WORLD_8 = 168,
      vk_WORLD_9 = 169,
      vk_WORLD_10 = 170,
      vk_WORLD_11 = 171,
      vk_WORLD_12 = 172,
      vk_WORLD_13 = 173,
      vk_WORLD_14 = 174,
      vk_WORLD_15 = 175,
      vk_WORLD_16 = 176,
      vk_WORLD_17 = 177,
      vk_WORLD_18 = 178,
      vk_WORLD_19 = 179,
      vk_WORLD_20 = 180,
      vk_WORLD_21 = 181,
      vk_WORLD_22 = 182,
      vk_WORLD_23 = 183,
      vk_WORLD_24 = 184,
      vk_WORLD_25 = 185,
      vk_WORLD_26 = 186,
      vk_WORLD_27 = 187,
      vk_WORLD_28 = 188,
      vk_WORLD_29 = 189,
      vk_WORLD_30 = 190,
      vk_WORLD_31 = 191,
      vk_WORLD_32 = 192,
      vk_WORLD_33 = 193,
      vk_WORLD_34 = 194,
      vk_WORLD_35 = 195,
      vk_WORLD_36 = 196,
      vk_WORLD_37 = 197,
      vk_WORLD_38 = 198,
      vk_WORLD_39 = 199,
      vk_WORLD_40 = 200,
      vk_WORLD_41 = 201,
      vk_WORLD_42 = 202,
      vk_WORLD_43 = 203,
      vk_WORLD_44 = 204,
      vk_WORLD_45 = 205,
      vk_WORLD_46 = 206,
      vk_WORLD_47 = 207,
      vk_WORLD_48 = 208,
      vk_WORLD_49 = 209,
      vk_WORLD_50 = 210,
      vk_WORLD_51 = 211,
      vk_WORLD_52 = 212,
      vk_WORLD_53 = 213,
      vk_WORLD_54 = 214,
      vk_WORLD_55 = 215,
      vk_WORLD_56 = 216,
      vk_WORLD_57 = 217,
      vk_WORLD_58 = 218,
      vk_WORLD_59 = 219,
      vk_WORLD_60 = 220,
      vk_WORLD_61 = 221,
      vk_WORLD_62 = 222,
      vk_WORLD_63 = 223,
      vk_WORLD_64 = 224,
      vk_WORLD_65 = 225,
      vk_WORLD_66 = 226,
      vk_WORLD_67 = 227,
      vk_WORLD_68 = 228,
      vk_WORLD_69 = 229,
      vk_WORLD_70 = 230,
      vk_WORLD_71 = 231,
      vk_WORLD_72 = 232,
      vk_WORLD_73 = 233,
      vk_WORLD_74 = 234,
      vk_WORLD_75 = 235,
      vk_WORLD_76 = 236,
      vk_WORLD_77 = 237,
      vk_WORLD_78 = 238,
      vk_WORLD_79 = 239,
      vk_WORLD_80 = 240,
      vk_WORLD_81 = 241,
      vk_WORLD_82 = 242,
      vk_WORLD_83 = 243,
      vk_WORLD_84 = 244,
      vk_WORLD_85 = 245,
      vk_WORLD_86 = 246,
      vk_WORLD_87 = 247,
      vk_WORLD_88 = 248,
      vk_WORLD_89 = 249,
      vk_WORLD_90 = 250,
      vk_WORLD_91 = 251,
      vk_WORLD_92 = 252,
      vk_WORLD_93 = 253,
      vk_WORLD_94 = 254,
      vk_WORLD_95 = 255, // 0xFF

      // Numeric keypad
      vk_KP0 = 256,
      vk_KP1 = 257,
      vk_KP2 = 258,
      vk_KP3 = 259,
      vk_KP4 = 260,
      vk_KP5 = 261,
      vk_KP6 = 262,
      vk_KP7 = 263,
      vk_KP8 = 264,
      vk_KP9 = 265,
      vk_KP_PERIOD = 266,
      vk_KP_DIVIDE = 267,
      vk_KP_MULTIPLY = 268,
      vk_KP_MINUS = 269,
      vk_KP_PLUS = 270,
      vk_KP_ENTER = 271,
      vk_KP_EQUALS = 272,

      // Arrows + Home/End pad
      vk_UP = 273,
      vk_DOWN = 274,
      vk_RIGHT = 275,
      vk_LEFT = 276,
      vk_INSERT = 277,
      vk_HOME = 278,
      vk_END = 279,
      vk_PAGEUP = 280,
      vk_PAGEDOWN = 281,

      // Function keys
      vk_F1 = 282,
      vk_F2 = 283,
      vk_F3 = 284,
      vk_F4 = 285,
      vk_F5 = 286,
      vk_F6 = 287,
      vk_F7 = 288,
      vk_F8 = 289,
      vk_F9 = 290,
      vk_F10 = 291,
      vk_F11 = 292,
      vk_F12 = 293,
      vk_F13 = 294,
      vk_F14 = 295,
      vk_F15 = 296,

      // Key state modifier keys
      vk_NUMLOCK = 300,
      vk_CAPSLOCK = 301,
      vk_SCROLLOCK = 302,
      vk_RSHIFT = 303,
      vk_LSHIFT = 304,
      vk_RCTRL = 305,
      vk_LCTRL = 306,
      vk_RALT = 307,
      vk_LALT = 308,
      vk_RMETA = 309,
      vk_LMETA = 310,
      vk_LSUPER = 311, // Left "Windows" key
      vk_RSUPER = 312, // Right "Windows" key
      vk_MODE = 313, // "Alt Gr" key
      vk_COMPOSE = 314, // Multi-key compose key

      // Miscellaneous function keys
      vk_HELP = 315,
      vk_PRINT = 316,
      vk_SYSREQ = 317,
      vk_BREAK = 318,
      vk_MENU = 319,
      vk_POWER = 320, // Power Macintosh power key
      vk_EURO = 321 // Some european keyboards
    );

    /// @enum MapTag
    MapTag = (
      MapTag1 = 0, MapTag2 = 1, MapTag3 = 2, MapTag4 = 3, MapTag5 = 4, MapTag6 = 5, MapTag7 = 6, MapTag8 = 7, MapTag9 = 8,
      MapTag10 = 9, MapTag11 = 10, MapTag12 = 11, MapTag13 = 12, MapTag14 = 13, MapTag15 = 14, MapTag16 = 15,
      MapTag17 = 16, MapTag18 = 17, MapTag19 = 18, MapTag20 = 19, MapTag21 = 20, MapTag22 = 21, MapTag23 = 22,
      MapTag24 = 23
    );

    /// @struct MapTile
    MapTile = record
      xIndex: LongInt;
      yIndex: LongInt;
      topCorner: Point2D;
      pointA: Point2D;
      pointB: Point2D;
      pointC: Point2D;
      pointD: Point2D;
    end;

    /// @struct MapData
    /// @via_pointer
    MapData = record
      Version: LongInt;
      MapWidth: LongInt;
      MapHeight: LongInt;
      BlockWidth: LongInt;
      BlockHeight: LongInt;
      NumberOfBlocks: LongInt;
      NumberOfLayers: LongInt;
      NumberOfAnimations: LongInt;
      CollisionLayer: LongInt;
      TagLayer: LongInt;
      GapX: LongInt;
      GapY: LongInt;
      StaggerX: LongInt;
      StaggerY: LongInt;
      Isometric: Boolean;
    end;

    /// @struct MapAnimationData
    /// @via_pointer
    MapAnimationData = record
      AnimationNumber: LongInt;
      Delay: LongInt;
      NumberOfFrames: LongInt;
      Frame: Array of LongInt;
      CurrentFrame: LongInt;
    end;

    /// @struct MapLayerData
    /// @via_pointer
    MapLayerData = record
      Animation: Array of Array of LongInt;
      Value: Array of Array of LongInt;
    end;

    /// @struct MapCollisionData
    /// @via_pointer
    MapCollisionData = record
      Collidable: Array of Array of Boolean;
    end;

    /// @struct MapTagDetails
    /// @via_pointer
    MapTagDetails = record
      x: LongInt;
      y: LongInt;
    end;

    /// @struct MapRecord
    /// @via_pointer
    MapRecord = record
      MapInfo: MapData;
      AnimationInfo: Array of MapAnimationData;
      LayerInfo: Array of MapLayerData;
      CollisionInfo: MapCollisionData;
      TagInfo: Array [0..23] of Array of MapTagDetails;
      Tiles: Sprite;
      Animate: Boolean;
      Frame: LongInt;
    end;

    /// @class Map
    /// @pointer_wrapper
    /// @field pointer: ^MapRecord
    Map = ^MapRecord;



    /// The FreeNotifier is a function pointer used to notify user programs of
    /// swingame resources being freed. This should not be used by user programs.
    ///
    /// @type FreeNotifier
    FreeNotifier = procedure (p: Pointer); cdecl;


//=============================================================================
implementation
//=============================================================================

uses sgShared;



//=============================================================================

initialization
begin
  InitialiseSwinGame();
end;

end.
