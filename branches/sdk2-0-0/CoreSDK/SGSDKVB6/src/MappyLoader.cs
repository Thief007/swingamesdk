using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace SwinGameVB
{
    [ComVisible(true)]
    public enum Event
    {
        Event1 = 0, Event2 = 1, Event3 = 2, Event4 = 3, Event5 = 4,
        Event6 = 5, Event7 = 6, Event8 = 7, Event9 = 8, Event10 = 9,
        Event11 = 10, Event12 = 11, Event13 = 12, Event14 = 13, Event15 = 14,
        Event16 = 15, Event17 = 16, Event18 = 17, Event19 = 18, Event20 = 19,
        Event21 = 20, Event22 = 21, Event23 = 22, Event24 = 23
    }
    [ComVisible(true)]
    public enum CollisionSide
    {
        Top,
        Bottom,
        Left,
        Right,
        TopLeft,
        TopRight,
        BottomLeft,
        BottomRight,
        LeftRight,
        TopBottom,
        None
    }
    
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("7BA5F37F-0F56-4339-91AC-78A1B5037694")]
    [ComVisible(true)]
    public class Map: IMap
    {
        private SwinGame.Map map;
        internal void Free()
        {
            SwinGame.MappyLoader.FreeMap(map);
        }
        internal SwinGame.Map result
        {
            get
            {
                return map;
            }
            set
            {
                map = value;
            }
        }
    }

    [Guid("A12666A7-11A9-446c-9542-D8F80A2B1EF9")]
    [ComVisible(true)]
    public interface IMap
    {
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("F13C547C-7712-4da4-9933-167B0A98264C")]
    [ComVisible(true)]
    public class MappyLoader : IMappyLoader
    {
        public Map LoadMap(String fileName)
        {
            Map map = new Map();
            map.result = SwinGame.MappyLoader.LoadMap(fileName);
            return map;
        }


        public void DrawMap(Map map)
        {
            SwinGame.MappyLoader.DrawMap(map.result);
        }

        public CollisionSide CollisionWithMapVector(Map map, Sprite spr, Vector vec)
        {
            return (CollisionSide)SwinGame.MappyLoader.CollisionWithMap(map.result, spr.result, vec.result);
        }

        public CollisionSide CollisionWithMap(Map map, Sprite spr)
        {
            return (CollisionSide)SwinGame.MappyLoader.CollisionWithMap(map.result, spr.result);
        }

        public int EventCount(Map map, Event evnt)
        {
            return SwinGame.MappyLoader.EventCount(map.result, (SwinGame.Event)evnt);
        }

        public int EventPositionX(Map map, Event evnt, int eventnumber)
        {
            return SwinGame.MappyLoader.EventPositionX(map.result, (SwinGame.Event)evnt, eventnumber);
        }

        public int EventPositionY(Map map, Event evnt, int eventnumber)
        {
            return SwinGame.MappyLoader.EventPositionY(map.result, (SwinGame.Event)evnt, eventnumber);
        }

        public void FreeMap(Map map)
        {
            map.Free();
        }



        public bool SpriteHasCollidedWithMapTile(Map map, Sprite spr, out int collidedX, out int collidedY)
        {
            return SwinGame.MappyLoader.SpriteHasCollidedWithMapTile(map.result, spr.result, out collidedX, out collidedY);
        }

        public bool SpriteHasCollidedWithMapTile(Map map, Sprite spr)
        {
            return SwinGame.MappyLoader.SpriteHasCollidedWithMapTile(map.result, spr.result);
        }


        public CollisionSide WillCollideOnSide(Map map, Sprite spr)
        {
            return (CollisionSide)SwinGame.MappyLoader.WillCollideOnSide(map.result, spr.result);
        }

        public void MoveSpriteOutOfTile(Map map, Sprite spr, int x, int y)
        {
            SwinGame.MappyLoader.MoveSpriteOutOfTile(map.result, spr.result, x, y);
        }
    }

    [Guid("81A275EA-6F70-48a6-9DFC-656A7E81DDD5")]
    [ComVisible(true)]
    public interface IMappyLoader
    {
        Map LoadMap(String fileName);
        void DrawMap(Map map);
        CollisionSide CollisionWithMapVector(Map map, Sprite spr, Vector vec);

        CollisionSide CollisionWithMap(Map map, Sprite spr);

        int EventCount(Map map, Event evnt);

        int EventPositionX(Map map, Event evnt, int eventnumber);

        int EventPositionY(Map map, Event evnt, int eventnumber);

        void FreeMap(Map Map);

        bool SpriteHasCollidedWithMapTile(Map map, Sprite spr, out int collidedX, out int collidedY);
        bool SpriteHasCollidedWithMapTile(Map map, Sprite spr);
        CollisionSide WillCollideOnSide(Map map, Sprite spr);
        void MoveSpriteOutOfTile(Map map, Sprite spr, int x, int y);
    }
}
