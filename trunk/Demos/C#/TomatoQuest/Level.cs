using System;
using System.Drawing;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Text;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;

namespace TomatoQuest
{
    public class Level
    {
        private Map _Map;

        //Returns the Map
        public Map Map
        {
            get { return _Map; }
        }

        //Level Constructor
        public Level(String levelName)
        {
            //Load the Level
            _Map = Resources.GameMap("Level1");
        }

        //Draw Level Method
        public void DrawLevel()
        {
            //Draws the map
            MappyLoader.DrawMap(_Map);
        }

        //Event Position X
        public int EventPositionX(Event eventType, int index)
        {
            return MappyLoader.EventPositionX(_Map, eventType, index);
        }

        //Event Position Y
        public int EventPositionY(Event eventType, int index)
        {
            return MappyLoader.EventPositionY(_Map, eventType, index);
        }

        //Event Count
        public int EventCount(Event eventType, int index)
        {
            return MappyLoader.EventCount(_Map, eventType);
        }
    }
}
