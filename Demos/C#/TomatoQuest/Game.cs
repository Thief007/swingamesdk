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
    public class Game
    {
        //Constant
        const Event PLAYERSPAWN = Event.Event1;

        //Loads the Map at object creation
        private Level _Map = new Level("Level1");

        //Load the Player
        private Character _Player;

        //Load the Player Controller
        private Controller _Controller = new Controller();

        //Game Constructor
        public Game()
        {
            _Player = new Character("Hero", _Map.EventPositionX(PLAYERSPAWN, 0), _Map.EventPositionY(PLAYERSPAWN, 0));
        }

        public void Run()
        {
            //Draw the Map
            _Map.DrawLevel();

            //Update Player with Controls
            _Controller.UpdateInput(_Player, _Map);

            //Draw Player
            _Player.DrawCharacter();

            //Make the Camera follow the Player
            Camera.FollowSprite(_Player.Sprite, 0, 0);

            //Draw the FrameRate
            Text.DrawFramerate(200, 0, Resources.GameFont("Courier"));  
        } 
    }
}
