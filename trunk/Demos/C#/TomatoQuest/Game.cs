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
        const Event ENEMY1SPAWN = Event.Event4;
        const Event ENEMY2SPAWN = Event.Event5;
        const Event BOSSSPAWN = Event.Event3;

        //Loads the Map at object creation
        private Level _Map = new Level("Level1");

        //Load the Player
        private Character _Player;

        //Healer
        private Healers _Healers;

        //Enemies
        private Enemy _Critters;
        private Enemy _Thieves;
        private Enemy _Leader;

        private List<Character> _AI;

        //Load the Player Controller
        private Controller _Controller = new Controller();

        //AI Controller
        private AIController _AIController = new AIController();

        //Load The User Interface
        private UserInterface _Interface = new UserInterface();

        //Random number generator
        private Random _RandomNumber = new Random(System.DateTime.Now.Millisecond);

        //List of Items
        private List<Item> _Items = new List<Item>();

        //Game Constructor
        public Game()
        {
            _Player = new Character("Hero", _Map.EventPositionX(PLAYERSPAWN, 0), _Map.EventPositionY(PLAYERSPAWN, 0), 10, 10, 10, 10, 10, true, true, true);
           
            //Load the Healers
            _Healers = new Healers("Healer", _Map, 1, 1, 1, 1, 1, HEALERSPAWN);

            //Load the Enemies
            _Critters = new Enemy("Critter", _Map, 5, 5, 4, 4, 5, ENEMY1SPAWN, 40);
            _Thieves = new Enemy("Thief", _Map, 10, 8, 8, 4, 8, ENEMY2SPAWN, 120);
            _Leader = new Enemy("ThiefLeader", _Map, 35, 45, 20, 20, 10, BOSSSPAWN, 1000);

            //Create the AI List
            _AI = new List<Character>();

            //Add the Healers to the total AI
            _AI.AddRange(_Healers.Characters);

            //Add the AI to the total AI
            _AI.AddRange(_Critters.Characters);
            _AI.AddRange(_Thieves.Characters);
            _AI.AddRange(_Leader.Characters);

            _Items.Add(new Item("Tomato"));

            Item.GiveCharacterItem(_Leader.Characters[0], _Items[0]);
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

            //Interact with Healers and Update Player Status
            Interaction.InteractWithHealers(_Player, _Healers.Characters);
            _Player.UpdateCharacterStatus();

            //Combat
            Combat.AIHitPlayer(_Player, _AI, _RandomNumber);
            Combat.PlayerHitAI(_Player, _AI, _RandomNumber);

            //Draw Items
            for (int i = 0; i < _Items.Count; i++)
            {
                _Items[i].DrawItem();
            }

            //Run User Interface
            _Interface.RunUI(_Player);

            //User Stat Page Notification
            Text.DrawTextOnScreen("Hit S to Open and Close the Stat Page", Color.White, Resources.GameFont("Courier"), 260, 460);

            //Draw the FrameRate
            Text.DrawFramerate(200, 0, Resources.GameFont("Courier"));  
        } 
    }
}
