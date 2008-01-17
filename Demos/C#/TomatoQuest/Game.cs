using System;
using System.Drawing;
using System.Collections.Generic;



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
        const Event HEALERSPAWN = Event.Event2;

        //Loads the Map at object creation
        private Level _Map = new Level("Level1");

        //Load the Player
        private Character _Player;
        private Healers _Healers;
        private List<Character> _AI;

        //Load the Player Controller
        private Controller _Controller = new Controller();

        //AI Controller
        private AIController _AIController = new AIController();

        //Load The User Interface
        private UserInterface _Interface = new UserInterface();

        //Game Constructor
        public Game()
        {
            _Player = new Character("Hero", _Map.EventPositionX(PLAYERSPAWN, 0), _Map.EventPositionY(PLAYERSPAWN, 0), 10, 10, 10, 10, 10, true, true, true);
           
            //Load the Healers
            _Healers = new Healers("Healer", _Map, 1, 1, 1, 1, 1, HEALERSPAWN);

            //Create the AI List
            _AI = new List<Character>();

            //Add the Healers to the total AI
            _AI.AddRange(_Healers.Characters);
        }

        public void Run()
        {
            //Draw the Map
            _Map.DrawLevel();

            //Update Player with Controls
            _Controller.UpdateInput(_Player, _Map);

            //Draw Player
            _Player.DrawCharacter();

            //Refresh Player Stats
            _Player.RefreshCharacterStats();

            //Make the Camera follow the Player
            Camera.FollowSprite(_Player.Sprite, 0, 0);

            //AI Controller
            _AIController.UpdateAI(_AI, _Player, _Map.Map);

            // If Player Collides with the AI, move them back.
            _AIController.PlayerCollideWithAI(_Player, _AI);

            //Run User Interface
            _Interface.RunUI(_Player);

            //User Stat Page Notification
            Text.DrawTextOnScreen("Hit S to Open and Close the Stat Page", Color.White, Resources.GameFont("Courier"), 260, 460);

            //Draw the FrameRate
            Text.DrawFramerate(200, 0, Resources.GameFont("Courier"));  
        } 
    }
}
