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
    public class Enemy
    {
        //An Array of Characters
        public List<Character> Characters = new List<Character>();

        public Enemy(String name, Level map, int Strength, int Vitality, int Agility, int Intelligence, int Luck, Event eventtype, int Experience)
        {
            //Goes through each healer and creates a new character for each one
            //Placing each healer at it's spawn point.
            //Also giving each Enemy, experience, that will given to the player when
            //the player defeats the enemy.
            for (int i = 0; i < MappyLoader.EventCount(map.Map, eventtype) - 1; i++)
            {
                Characters.Add(new Character(name, map.EventPositionX(eventtype, i), map.EventPositionY(eventtype, i), Strength, Vitality, Agility, Intelligence, Luck, true, true, false));
                Characters[i].Experience = Experience;
            }
        }
    }
}
