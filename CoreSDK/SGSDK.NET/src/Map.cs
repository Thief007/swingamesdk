using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// A Map is a tile based collection of sprites, that form a complete map. Map's can be animated,
    /// and maps that are animated, animate automatically. Collisions between sprite and maps can also be
    /// done, so your sprites do not go into collidable areas of the map.
    /// </summary>
    public class Map : IDisposable
    {
        internal SwinGamePointer Pointer;

        internal Map(IntPtr devPtr) : this(devPtr, false) {}

        internal Map(IntPtr devPtr, bool isCopy)
        {
            if(isCopy)
                Pointer = new SwinGamePointer(devPtr, PtrKind.Copy);
            else
                Pointer = new SwinGamePointer(devPtr, PtrKind.Image);
        }

        /// <summary>
        /// Loads a Map, using mapFile to indicate which map file to load.
        /// This requires that the .sga and .png files for the map
        /// are located in the resources folder of the application.
        /// </summary>
        /// <param name="mapName">Name of the map</param>
        /// <returns>Map</returns>
        public Map(string mapName)
        {
            string mapFile = Core.GetPathToResource(mapName + ".sga", ResourceKind.MapResource);
            string imgFile = Core.GetPathToResource(mapName + ".png", ResourceKind.MapResource);

            Pointer = new SwinGamePointer(SGSDK.LoadMap(mapFile, imgFile), PtrKind.Map);
        }

        /// <summary>
        /// Draws the specified Map
        /// </summary>
        /// <param name="map">Map to be drawn to the screen</param>
        public void Draw()
        {
            SGSDK.DrawMap(this);
        }

        /// <summary>
        /// This function checks if the specified sprite has collided with the specified map.
        /// This function will also move the sprite outside of the map automatically.
        /// Useful for checking collisions, getting which side the collision has occured on, and for
        /// moving the sprite out of the collidable areas of the map.
        /// This overload allows you to pass in a vector, which describes the movement of the sprite, if the sprites movement vector has
        /// not been used.
        /// </summary>
        /// <param name="spr">Sprite to be checked for collisions</param>
        /// <param name="vec">Vector to be used</param>
        /// <returns>Collision Side</returns>
        public CollisionSide CollisionWithMap(Sprite spr, Vector vec)
        {
            return (CollisionSide)SGSDK.CollisionWithMapVector(this, spr, vec);
        }

        /// <summary>
        /// This function checks if the specified sprite has collided with the specified map.
        /// This function will also move the sprite outside of the map automatically.
        /// Useful for checking collisions, getting which side the collision has occured on, and for
        /// moving the sprite out of the collidable areas of the map.
        /// </summary>
        /// <param name="spr">Sprite to be checked for collisions</param>
        /// <returns>Collision Side</returns>
        public CollisionSide CollisionWithMap(Sprite spr)
        {
            return (CollisionSide)SGSDK.CollisionWithMapVector(this, spr, spr.Movement);
        }

        /// <summary>
        /// This function returns the number of the specified Event type found in the Map specified.
        /// </summary>
        /// <param name="evnt">Event Type to be checked</param>
        /// <returns>Number of events of Event Type specified</returns>
        public int EventCount(Event evnt)
        {
            return SGSDK.EventCount(this, evnt);
        }

        /// <summary>
        /// Gets the X Coordinate of the Event specified. evnt is the Event Type that is to be searched
        /// for, and eventnumber is the index of the event, since there can be more then 1 of each Event
        /// Type.
        /// </summary>
        /// <param name="map">Map containing the Event</param>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game X Coordinate</returns>
        public int EventPositionX(Event evnt, int eventnumber)
        {
            return SGSDK.EventPositionX(this, evnt, eventnumber);
        }

        /// <summary>
        /// Gets the Y Coordinate of the Event specified. evnt is the Event Type that is to be searched
        /// for, and eventnumber is the index of the event, since there can be more then 1 of each Event
        /// Type.
        /// </summary>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game Y Coordinate</returns>
        public int EventPositionY(Event evnt, int eventnumber)
        {
            return SGSDK.EventPositionY(this, evnt, eventnumber);
        }

        /// <summary>
        /// Gets the Coordinates of the Event specified. evnt is the Event Type that is to be searched
        /// for, and eventnumber is the index of the event, since there can be more then 1 of each Event
        /// Type.
        /// </summary>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventNumber">Event Index</param>
        /// <returns>Game Coordinate of the event instance</returns>
        public Point2D EventPosition(Event evnt, int eventNumber)
        {
            Point2D p = new Point2D();

            p.X = EventPositionX(evnt, eventNumber);
            p.Y = EventPositionY(evnt, eventNumber);

            return p;
        }

        /// <summary>
        /// Checks whether the specified Sprite has collided with a map tile within the specified map.
        /// If so, this method returns true, else false. This command can be used to determine whether
        /// a Sprite has collided with the map, but also to find the x and y coordinate of the tile within
        /// the map it collided with.
        /// </summary>
        /// <param name="spr">The Sprite to be checked against the map</param>
        /// <param name="collidedX">The X Coordinate of the Tile that Sprite has collided with</param>
        /// <param name="collidedY">The Y Coordinate of the Tile that Sprite has collided with</param>
        /// <returns></returns>
        public bool SpriteHasCollidedWithMapTile(Sprite spr, out int collidedX, out int collidedY)
        {
            return SGSDK.SpriteHasCollidedWithMapTile(this, spr, out collidedX, out collidedY) == -1;
        }

        /// <summary>
        /// Checks whether the specified Sprite has collided with a map tile within the specified map.
        /// If so, this method returns true, else false. This command can be used to determine whether
        /// a Sprite has collided with the map.
        /// </summary>
        /// <param name="spr">The Sprite to be checked against the map</param>
        /// <returns></returns>
        public bool SpriteHasCollidedWithMapTile(Sprite spr)
        {
            int collidedX;
            int collidedY;

            return SGSDK.SpriteHasCollidedWithMapTile(this, spr.Pointer, out collidedX, out collidedY) == -1;
        }

        /// <summary>
        /// This function with find which side the Collision between a Sprite and Map has occurred on.
        /// This can be used to make different choices depending on which side of the Map the Sprite has
        /// hit.
        /// </summary>
        /// <param name="spr">The sprite to be checke against collisions</param>
        /// <returns></returns>
        public CollisionSide WillCollideOnSide(Sprite spr)
        {
            return (CollisionSide)SGSDK.WillCollideOnSide(this, spr);
        }

       /// <summary>
        /// This routine with move a specified sprite out the map's tile that is specified. This routine
        /// moves the sprite so that is just outside of the tile in question.
        /// </summary>
        /// <param name="spr">Sprite to be moved out</param>
        /// <param name="x">X Coordinate of the tile</param>
        /// <param name="y">Y Coordinate of the tile</param>
        /// <returns></returns>
        public void MoveSpriteOutOfTile(Sprite spr, int x, int y)
        {
            SGSDK.MoveSpriteOutOfTile(this, spr, x, y);
        }

        /// <summary>
        /// This function will return the width of the map in tiles.
        /// Example: using a 10x12 map, this function will return 10.
        /// </summary>
        public int Width
        {
            get
            {
                return SGSDK.MapWidth(this);
            }
        }

        /// <summary>
        /// This function will return the height of the map in tiles.
        /// Example: using a 10x12 map, this function will return 12.
        /// </summary>
        public int Height
        {
            get
            {
                return SGSDK.MapHeight(this);
            }
        }

        /// <summary>
        /// This function will return the width of the map blocks
        /// </summary>
        public int BlockWidth
        {
            get
            {
                return SGSDK.BlockWidth(this);
            }
        }

        /// <summary>
        /// This function will return the width of the map blocks
        /// </summary>
        public int BlockHeight
        {
            get
            {
                return SGSDK.BlockHeight(this);
            }
        }

        /// <summary>
        /// This function will return the horizontal gap between tiles in ISOMETRIC maps
        /// </summary>
        public int GapX
        {
            get
            {
                return SGSDK.GapX(this);
            }
        }

        /// <summary>
        /// This function will return the vertical gap between tiles in ISOMETRIC maps
        /// </summary>
        public int GapY
        {
            get
            {
                return SGSDK.GapY(this);
            }
        }

        /// <summary>
        /// This function will return the horizontal offset for ISOMETRIC maps
        /// </summary>
        public int StaggerX
        {
            get
            {
                return SGSDK.StaggerX(this);
            }
        }

        /// <summary>
        /// This function will return the vertical offset for ISOMETRIC maps
        /// </summary>
        public int StaggerY
        {
            get
            {
                return SGSDK.StaggerY(this);
            }
        }

        /// <summary>
        /// This function will get the tile that contains the given point, if no tile exists that
        /// contains the point, a tile with both indexes as -1 and Points(0,0) will be returned.
        /// </summary>
        /// <param name="point">Point</param>
        public Tile GetTileFromPoint(Point2D point)
        {
            return SGSDK.GetTileFromPoint(point, this);
        }

        /// <summary>
        /// This function will return the Event of the tile within the map. The number 255 will be
        /// returned if there is no event on this tile, or there is no tile at the given x and y index
        /// </summary>
        /// <param name="xIndex">The X Index of the Tile</param>
        /// <param name="yIndex">The Y Index of the Tile</param>
        public Event GetEventAtTile(int xIndex, int yIndex)
        {
            return (Event)SGSDK.GetEventAtTile(this, xIndex, yIndex);
        }


        /// <summary>
        /// Casts the Map to its native pointer.
        /// </summary>
        /// <param name="effect">the effect to cast</param>
        /// <returns>the native pointer</returns>
        public static implicit operator IntPtr(Map effect)
        {
            return effect.Pointer.Pointer;
        }

        #region IDisposable Members

        /// <summary>
        /// Clean up the native resources used by this sound effect.
        /// </summary>
        public void Dispose()
        {
            if (Pointer != null) Pointer.Free();
        }
        #endregion

    }
}
