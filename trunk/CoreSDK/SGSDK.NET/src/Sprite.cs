using System;
using System.Collections.Generic;
using System.Text;
using Rectangle = System.Drawing.Rectangle;

namespace SwinGame
{
    /// <summary>
    /// This contains number of bitmaps and its position.
    /// </summary>
    public class Sprite : IDisposable
    {
        //internal IntPtr Pointer;
        readonly internal SwinGamePointer Pointer;
        readonly List<Bitmap> _bitmaps = new List<Bitmap>();

        internal Sprite(IntPtr devPtr)
        {
            Pointer = new SwinGamePointer(devPtr, PtrKind.Sprite);
        }

        /// <summary>
        /// Creates a basic sprite, and sets its first bitmap. Use this for non-animating sprites.
        /// </summary>
        /// <param name="startBitmap">The sprites first bitmap (index 0)</param>
        public Sprite(Bitmap startBitmap)
        {
            Pointer = new SwinGamePointer(SGSDK.CreateSprite(startBitmap), PtrKind.Sprite);
            _bitmaps.Add(startBitmap);
        }

        /// <summary>
        /// Creates a new Sprite. This version allows you to chose if you want a multi cell bitmap 
        /// and pick the ending action
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        public Sprite(Bitmap startBitmap, bool isMulti, int[] framesPerCell, SpriteEndingAction endingAction, int width, int height)
        {
            Pointer = new SwinGamePointer(SGSDK.CreateSpriteMultiEnding(startBitmap.pointer, (isMulti ? -1 : 0), framesPerCell.Length, framesPerCell, (int)endingAction, width, height), PtrKind.Sprite);
            _bitmaps.Add(startBitmap);
        }

        /// <summary>
        /// Creates a new Sprite. This version allows you to chose if you want a multi cell bitmap
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        public Sprite(Bitmap startBitmap, bool isMulti, int[] framesPerCell, int width, int height)
        {
            Pointer = new SwinGamePointer(SGSDK.CreateSpriteMulti(startBitmap.pointer, (isMulti ? -1 : 0), framesPerCell.Length, framesPerCell, width, height), PtrKind.Sprite);
            _bitmaps.Add(startBitmap);
        }

        /// <summary>
        /// Creates a Sprite. This version allows you to pick the frames per cell
        /// </summary>
        /// <param name="startBitmap">StartBitmap</param>
        /// <param name="framesPerCell">Delay each frame</param>
        /// <param name="frames">number of Frames</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public Sprite(Bitmap startBitmap, int framesPerCell, int frames, int width, int height)
        {
            Pointer = new SwinGamePointer(SGSDK.CreateSpriteMultiFPC(startBitmap.pointer, framesPerCell, frames, width, height), PtrKind.Sprite);
            _bitmaps.Add(startBitmap);
        }

        /// <summary>
        /// Creates a new Sprite. This version allows you to set how many times each frame is drawn
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets how many times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        public Sprite(Bitmap[] startBitmap, int[] framesPerCell, SpriteEndingAction endingAction)
        {
            IntPtr[] temp = new IntPtr[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].pointer;
            }

            Pointer = new SwinGamePointer(SGSDK.CreateSpriteArrayEnding(startBitmap.Length, temp, framesPerCell.Length, framesPerCell, (int)endingAction), PtrKind.Sprite);
            _bitmaps.AddRange(startBitmap);
        }

        /// <summary>
        /// Creates a new Sprite with an array of bitmaps
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        public Sprite(Bitmap[] startBitmap, int[] framesPerCell)
        {
            IntPtr[] temp = new IntPtr[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].pointer;            
            }

            Pointer = new SwinGamePointer(SGSDK.CreateSpriteArray(temp.Length, temp, framesPerCell.Length, framesPerCell), PtrKind.Sprite);
            _bitmaps.AddRange(startBitmap);
        }

        /// <summary>
        /// Creates a Sprite with an array of bitmaps and the frames per cell
        /// </summary>
        /// <param name="startBitmap">Start Bitmap</param>
        /// <param name="framesPerCell">Delay per Frame</param>
        /// <param name="frames">Number of Frames</param>
        public Sprite(Bitmap[] startBitmap, int framesPerCell, int frames)
        {
            IntPtr[] temp = new IntPtr[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].pointer;
            }
            
            Pointer = new SwinGamePointer(SGSDK.CreateSpriteArrayFPC(temp.Length, temp, framesPerCell, frames), PtrKind.Sprite);
            _bitmaps.AddRange(startBitmap);
        }

        /// <summary>
        /// Updates the Sprites Animation, this will move to the next frame in the sprite
        /// </summary>
        public void UpdateAnimation()
        {
            SGSDK.UpdateSpriteAnimation(this);
        }

        /// <summary>
        /// Casts the Sprite to its native pointer.
        /// </summary>
        /// <param name="sprt">the sprite to cast</param>
        /// <returns>the native pointer to the sprite</returns>
        public static implicit operator IntPtr(Sprite sprt)
        {
            return sprt.Pointer.Pointer;
        }

        /// <summary>
        /// Sprites may contain multiple images. These images can be used for things
        ///	line animation, facing, etc. This routine adds a bitmap to a sprite,
        ///	returning the index of the added bitmap.
        /// </summary>
        /// <param name="bitmapToAdd">the bitmap to add to the sprite</param>
        /// <returns>the index of the added bitmap</returns>
        public int AddBitmap(Bitmap bitmapToAdd)
        {
            _bitmaps.Add(bitmapToAdd);
            return SGSDK.AddBitmapToSprite(this, bitmapToAdd);
        }

        /// <summary>
        /// Returns the current height of the sprite
        /// </summary>
        /// <returns>The height of the sprite's current frame</returns>
        public int Height
        {
            get
            {
                return SGSDK.CurrentHeight(this);
            }
        }

        /// <summary>
        /// Returns the current width of the sprite
        /// </summary>
        /// <returns>The width of the sprite's current frame</returns>
        public int Width
        {
            get
            {
                return SGSDK.CurrentWidth(this);
            }
        }

        /// <summary>
        /// Draws a sprite to the game screen
        /// </summary>
        public void Draw()
        {
            Draw(0, 0);
        }

        /// <summary>
        /// Draws the sprite with an offset.
        /// </summary>
        /// <param name="xOffset">the x offset of the sprite when drawn</param>
        /// <param name="yOffset">the y offset of the sprite when drawn</param>
        public void Draw(int xOffset, int yOffset)
        {
            SGSDK.DrawSprite(this, xOffset, yOffset);
        }

        /// <summary>
        /// Moves a sprite based on information in a movement vector
        /// </summary>
        /// <param name="movementVector">The vector containing the movement details</param>
        public void Move(Vector movementVector)
        {
            SGSDK.MoveSprite(this, movementVector);
        }

        /// <summary>
        /// Moves a sprite based on the movement vector that is associated
        /// with the sprite.
        /// </summary>
        public void Move()
        {
            SGSDK.MoveSpriteItself(this);
        }

        /// <summary>
        /// Moves a sprite to a given x,y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="x">the new location of the sprite</param>
        /// <param name="y">the new location of the sprite</param>
        public void MoveTo(int x, int y)
        {
            SGSDK.MoveSpriteTo(this, x, y);
        }

        /// <summary>
        /// Determines if a sprite is off the screen
        /// </summary>
        /// <returns>True if the sprite is off the screen</returns>
        public bool IsOffscreen()
        {
            return SGSDK.IsSpriteOffscreen(this) == -1;
        }
                

        /// <summary>
        /// Clean up system resources.
        /// </summary>
        public void Dispose()
        {
            Pointer.Free();
            _bitmaps.Clear();
        }

        /*[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpritehasEnded(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteReverse(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteBitmap")]
        private static extern IntPtr DLL_GetSpriteBitmap(IntPtr pointer, int id);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteX")]
        private static extern float DLL_GetSpriteX(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteX")]
        private static extern void DLL_SetSpriteX(IntPtr pointer, float X);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteY")]
        private static extern float DLL_GetSpriteY(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteY")]
        private static extern void DLL_SetSpriteY(IntPtr pointer, float Y);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteCurrentFrame")]
        private static extern int DLL_GetSpriteCurrentFrame(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteCurrentFrame")]
        private static extern void DLL_SetSpriteCurrentFrame(IntPtr pointer, int frame);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteUsePixelCollision")]
        private static extern bool DLL_GetSpriteUsePixelCollision(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteUsePixelCollision")]
        private static extern void DLL_SetSpriteUsePixelCollision(IntPtr pointer, int pixelcollision);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteMass")]
        private static extern float DLL_GetSpriteMass(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetSpriteMovement")]
        private static extern Vector DLL_GetSpriteMovement(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteMass")]
        private static extern void DLL_SetSpriteMass(IntPtr pointer, Single mass);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetSpriteMovement")]
        private static extern void DLL_SetSpriteMovement(IntPtr pointer, Vector movement);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteKind(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteKind(IntPtr pointer, int kind);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteFramesPerCell(IntPtr pointer, int index);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteFramesPerCell(IntPtr pointer, [MarshalAs(UnmanagedType.LPArray)]int[] framesPerCell, int length);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteCols(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteRow(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteFrameCount(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteEndingAction(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteEndingAction(IntPtr pointer, int endingAction);
        */

        /// <summary>
        /// Gets a Sprite's Bitmap
        /// </summary>
        /// <param name="id">Id</param>
        /// <returns>Bitmap</returns>
        private Bitmap GetSpriteBitmap(int id)
        {
            return _bitmaps[id];

            //Via DLL!
            //return new Bitmap(new SwinGamePointer(SGSDK.GetSpriteBitmap(this, id)));
        }

        /// <summary>
        /// Array of bitmaps this sprite contains
        /// </summary>
        /// <param name="idx">Index number</param>
        /// <returns>Bitmap of the specified frame</returns>
        public Bitmap this[int idx]
        {
            get
            {
                return GetSpriteBitmap(idx);
            }
        }

        /// <summary>
        /// X position of this sprite
        /// </summary>
        public float X
        {
            get
            {
                return SGSDK.GetSpriteX(this);
            }
            set
            {
                SGSDK.SetSpriteX(this, value);
            }
        }
        /// <summary>
        /// Y position of this sprite
        /// </summary>
        public float Y
        {
            get
            {
                return SGSDK.GetSpriteY(Pointer);
            }
            set
            {
                SGSDK.SetSpriteY(Pointer, value);
            }
        }

        /// <summary>
        /// Current animation frame of this sprite
        /// </summary>
        public int CurrentFrame
        {
            get
            {
                return SGSDK.GetSpriteCurrentFrame(Pointer);
            }
            set
            {
                SGSDK.SetSpriteCurrentFrame(Pointer, value);
            }
        }

        /// <summary>
        /// True if this sprite use pixel collision
        /// </summary>
        public bool UsePixelCollision
        {
            get
            {
                return SGSDK.GetSpriteUsePixelCollision(Pointer) == -1;
            }
            set
            {
                SGSDK.SetSpriteUsePixelCollision(Pointer, (value ? -1 : 0));
            }
        }

        /// <summary>
        /// Gets the Sprite Kind of this sprite
        /// </summary>
        public SpriteKind SpriteKind
        {
            get
            {
                return (SpriteKind)SGSDK.GetSpriteKind(Pointer);
            }
            set
            {
                SGSDK.SetSpriteKind(Pointer, (int)value);
            }
        }

        /// <summary>
        /// Gets the Frames per Cell of this sprite
        /// </summary>
        public int[] FramesPerCell
        {
            //TODO: This needs a wrapper to allow sprt.FramesPerCell[0] = 10; etc...
            get
            {
                int fc = FrameCount;
                int[] temparray = new int[FrameCount];

                for (int i = 0; i < fc; i++)
                {
                    temparray[i] = SGSDK.GetSpriteFramesPerCell(Pointer, i);
                }

                return temparray;
            }
            set
            {
                SGSDK.SetSpriteFramesPerCell(Pointer, value, value.Length);
            }
        }

        /// <summary>
        /// Gets the number of Columns of this sprite
        /// </summary>
        public int Cols
        {
            get
            {
                return SGSDK.GetSpriteCols(Pointer);
            }
        }

        /// <summary>
        /// Gets the number of Rows of this sprite
        /// </summary>
        public int Rows
        {
            get
            {
                return SGSDK.GetSpriteRow(Pointer);
            }
        }

        /// <summary>
        /// Gets the Frame Count of this sprite
        /// </summary>
        public int FrameCount
        {
            get
            {
                return SGSDK.GetSpriteFrameCount(Pointer);
            }
        }

        /// <summary>
        /// Gets the Ending Action of this sprite
        /// </summary>
        public SpriteEndingAction EndingAction
        {
            get
            {
                return (SpriteEndingAction)SGSDK.GetSpriteEndingAction(Pointer);
            }
            set
            {
                SGSDK.SetSpriteEndingAction(Pointer, (int)value);
            }
        }

        /// <summary>
        /// Gets whether the Sprite Animation has Ended
        /// </summary>
        public Boolean hasEnded
        {
            get
            {
                return SGSDK.GetSpritehasEnded(Pointer) == -1;
            }
        }

        /// <summary>
        /// Gets whether the Sprite is reversed
        /// </summary>
        public bool Reverse
        {
            get
            {
                return SGSDK.GetSpriteReverse(Pointer) == -1;
            }
        }

        /// <summary>
        /// The movement of this sprite
        /// </summary>
        public MovementClass Movement
        {
            get
            {
                return new MovementClass(this);
            }
            set
            {
                SGSDK.SetSpriteMovement(Pointer, value.GetVectData());
            }
        }

        /// <summary>
        /// Constructor of this sprite
        /// </summary>
        public class MovementClass
        {
            private Sprite _Data;
            private Vector _Vector;

            internal MovementClass(Sprite data)
            {
                _Data = data;
            }

            internal MovementClass(Vector v) //for Vector cast to MovementClass
            {
                _Data = null;
                _Vector = v;
            }

            internal Vector GetVectData()
            {
                if (_Data == null)
                    return _Vector;
                else
                    return AsVector();
            }

            /// <summary>
            /// The movement of this sprite in the x direction
            /// </summary>
            public float X
            {
                get
                {
                    return SGSDK.GetSpriteMovement(_Data.Pointer).X;
                }
                set
                {
                    Vector v = SGSDK.GetSpriteMovement(_Data.Pointer);
                    v.X = value;
                    SGSDK.SetSpriteMovement(_Data.Pointer, v);
                }
            }

            /// <summary>
            /// The movement of this sprite in the y direction
            /// </summary>
            public float Y
            {
                get
                {
                    return SGSDK.GetSpriteMovement(_Data.Pointer).Y;
                }
                set
                {
                    Vector v = SGSDK.GetSpriteMovement(_Data.Pointer);
                    v.Y = value;
                    SGSDK.SetSpriteMovement(_Data.Pointer, v);
                }
            }

            /// <summary>
            /// Sets the Movement of this sprite
            /// </summary>
            /// <param name="v"></param>
            public void SetTo(Vector v)
            {
                SGSDK.SetSpriteMovement(_Data.Pointer, v);
            }

            /// <summary>
            /// Implicit cast of the Movement to a Vector
            /// </summary>
            /// <param name="mc">Movement Class</param>
            /// <returns>Vector</returns>
            public static implicit operator Vector(MovementClass mc)
            {
                return mc.AsVector();
            }

            public Vector AsVector()
            {
                return SGSDK.GetSpriteMovement(_Data.Pointer);
            }
        }

        /// <summary>
        /// Gets and Sets this Mass
        /// </summary>
        public float Mass
        {
            get
            {
                return SGSDK.GetSpriteMass(Pointer);
            }
            set
            {
                SGSDK.SetSpriteMass(Pointer, value);
            }
        }

        /// <summary>
        /// Replays a Sprite's Animation, if it has stopped
        /// </summary>
        public void ReplayAnimation()
        {
            SGSDK.ReplayAnimation(this);
        }

        /// <summary>
        /// Updates the Sprites Animation and Movement
        /// </summary>
        public void Update()
        {
            SGSDK.UpdateSprite(this);
        }

        ///<summary>
        /// Updates a sprite a given percentage
        ///</summary>
        ///<param name="pct">the percentage to update the sprite, 1 = 100%</param>
        public void Update(float pct)
        {
            SGSDK.UpdateSpritePct(this, pct);
        }

        ///<summary>
        /// Updates a sprite's animation by a given percentage
        ///</summary>
        ///<param name="pct">the percentage to update the sprite, 1 = 100%</param>
        public void UpdateAnimation(float pct)
        {
            SGSDK.UpdateSpriteAnimationPct(this, pct);
        }

        /// <summary>
        /// Move the Camera to center on the Sprite. This must be called each time
        /// you move the sprite if you want the camera to follow that sprite. The offsets
        /// allow you to move the sprite from direct center, for example if yOffset is set
        /// to +10 the sprite will appear 10 pixels below center.
        /// </summary>
        /// <param name="xOffset">The x offset from center</param>
        /// <param name="yOffset">The y offset from center</param>
        public void CenterCamera(int xOffset, int yOffset)
        {
            SGSDK.FollowSprite(this, xOffset, yOffset);
        }

        /// <summary>
        /// Determines if a sprite has collided with a given x position.
        /// </summary>
        /// <param name="x">The x location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public bool HasCollidedX(int x, CollisionDetectionRange range)
        {
            return SGSDK.HasSpriteCollidedX(this, x, (int)range) == -1;
        }

        /// <summary>
        /// Determines if a sprite has collided with a given y position.
        /// </summary>
        /// <param name="y">The y location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public bool HasCollidedY(int y, CollisionDetectionRange range)
        {
            return SGSDK.HasSpriteCollidedY(this, y, (int)range) == -1;
        }

        /// <summary>
        /// Determined if a sprite has collided with a given rectangle. The rectangles
        ///	coordinates are expressed in "world" coordinates.
        /// </summary>
        /// <param name="x">The x location of the rectangle</param>
        /// <param name="y">The y location of the rectangle</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public bool HasCollidedWithRect(Single x, Single y, int width, int height)
        {
            return SGSDK.HasSpriteCollidedWithRect(this, x, y, width, height) == -1;
        }

        /// <summary>
        /// Determined if a sprite has collided with a given rectangle. The rectangles
        ///	coordinates must be expressed in "game" coordinates.
        /// </summary>
        /// <param name="rect">The rectangle to check collisions with</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public bool HasCollidedWithRect(Rectangle rect)
        {
            return HasCollidedWithRect(rect.X, rect.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Determines if two sprites have collided. Sprites have collided when
        /// their images overlap. You may want to consider separating these if you
        /// are performing any "bounce" like activities. Separation is part of the
        /// collision code in many cases.
        /// </summary>
        /// <param name="other">The other sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public bool HasCollidedWith(Sprite other)
        {
            return SGSDK.HaveSpritesCollided(this, other) == -1;
        }

        /// <summary>
        /// Determines if a sprite has collided with a bitmap using pixel level
        ///	collision detection with the bitmap.
        /// </summary>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <param name="bounded">Indicates if theBitmap should use bounded collision</param>
        /// <returns>True if the bitmap has collided with the sprite.</returns>
        public bool HasCollidedWithBitmap(Bitmap theBitmap, float x, float y, bool bounded)
        {
            return SGSDK.HasSpriteCollidedWithBitmap(this, theBitmap, x, y, (bounded ? -1 : 0)) == -1;
        }

        /// <summary>
        /// Determines if a sprte has collided with a bitmap. This version is used to check
        /// for collisions with a cell from the bitmap. The src rectangle defines the part of
        /// the bitmap you want to check collisions with. This is usefull for checking
        /// collisions between sprites and bitmaps you use to contain multiple cells
        /// for animation etc.
        /// </summary>
        /// <param name="bmp">the bitmap containing the cell you want to check</param>
        /// <param name="pt">the pt, in game coordinates, of the bitmap cell</param>
        /// <param name="src">the rectangle containing the portion of the bitmap to check</param>
        /// <param name="bounded">set to true to perform a bounded box check, false for per pixel checks</param>
        /// <returns>true if the sprite has collided with the bitmap cell</returns>
        public bool HasCollidedWithBitmap(Bitmap bmp, Point2D pt, Rectangle src, bool bounded)
        {
            return SGSDK.HasSpriteCollidedWithBitmapPart(this, bmp, pt, Shapes.ToSGSDKRect(src), (bounded ? -1 : 0)) == -1;
        }

        /// <summary>
        /// Determines if a sprte has collided with a bitmap. This version is used to check
        /// for collisions with a cell from the bitmap. The src rectangle defines the part of
        /// the bitmap you want to check collisions with. This is usefull for checking
        /// collisions between sprites and bitmaps you use to contain multiple cells
        /// for animation etc.
        /// </summary>
        /// <param name="bmp">the bitmap containing the cell you want to check</param>
        /// <param name="pt">the pt, in game coordinates, of the bitmap cell</param>
        /// <param name="bounded">set to true to perform a bounded box check, false for per pixel checks</param>
        /// <returns>true if the sprite has collided with the bitmap cell</returns>
        public bool HasCollidedWithBitmap(Bitmap bmp, Point2D pt, bool bounded)
        {
            return HasCollidedWithBitmap(bmp, pt, Shapes.CreateRectangle(bmp), bounded);
        }

        /// <summary>
        /// Creates and returns the vector from centre of the sprite to the point.
        /// </summary>
        /// <param name="pnt">Point</param>
        /// <returns>Vector from Sprite to Point</returns>
        public Vector VectorFromCenterToPoint(Point2D pnt)
        {
            return Physics.VectorFromPoints(CenterPoint, pnt);
        }

        /// <summary>
        /// Returns the center point of the sprite. This does not take into
        /// consideration the shape of the bitmap being drawn, just returns
        /// the center point of the sprites bounding rectangle.
        /// </summary>
        /// <param name="sprt">The sprite to get the center point of.</param>
        /// <returns></returns>
        public Point2D CenterPoint
        {
            get
            {
                return SGSDK.CenterPoint(this);
            }
        }

        /// <summary>
        /// Checks if the Sprite is on Screen at the given Coordinates
        /// </summary>
        /// <param name="x">X Coordinate</param>
        /// <param name="y">Y Coordinate</param>
        /// <returns>True if the sprite is on screen at the coordinates</returns>
        public bool IsOnScreenAt(int x, int y)
        {
            return SGSDK.IsSpriteOnScreenAt(this, x, y) == -1;
        }

        /// <summary>
        /// Checks if the Sprite is on Screen at the given Coordinates
        /// </summary>
        /// <param name="point">Coordinates</param>
        /// <returns>True if the Sprite is on Screen at the Coordinates</returns>
        public bool IsOnScreenAt(Point2D point)
        {
            return IsOnScreenAt((int)point.X, (int)point.Y);
        }

        /// <summary>
        /// Calculates the Vector to get from the first Sprite to the second
        /// </summary>
        /// <param name="dest">Sprite to end the Vector</param>
        /// <returns>Vector from Sprite 1 to Sprite 2</returns>
        public Vector VectorTo(Sprite dest)
        {
            return Physics.VectorFromPoints(CenterPoint, dest.CenterPoint);
        }

    }
}
