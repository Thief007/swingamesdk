using System;
using System.Text;
using System.Drawing;
using System.Collections;
using System.Collections.Generic;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;
using Queen = GameProject.Queen;

using GameResources;

namespace GameProject
{
    public class Diagonal
    {
        public int Queens;
        
        public int Conflicts
        {
            get { return Queens - 1; }
        }
    }
}