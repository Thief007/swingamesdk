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
    public class Character
    {
        //Constant
        const Event PLAYERSPAWN = Event.Event1;

        private Sprite _Sprite;

        public Sprite Sprite
        {
            get { return _Sprite; }
        }

        //Character Constructor
        public Character(String name, int SpawnX, int SpawnY)
        {
            //Load the Character Sprite
            _Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);

            //Position the Character
            _Sprite.xPos = SpawnX;
            _Sprite.yPos = SpawnY;
        }

        public void DrawCharacter()
        {
            //Draw the Character to the Screen
            Graphics.DrawSprite(_Sprite);
        }

    }
}

