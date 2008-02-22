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
    public class Healers
    {
        //An Array of Characters
        public List<Character> Characters = new List<Character>(); 

        public Healers(String name, Level map, int Strength, int Vitality, int Agility, int Intelligence, int Luck, Event eventtype)
        {
            //Goes through each healer and creates a new character for each one
            //Placing each healer at it's spawn point.
            for (int i = 0; i < MappyLoader.EventCount(map.Map, eventtype); i++)
            {
                Characters.Add(new Character("Healer", map.EventPositionX(eventtype, i), map.EventPositionY(eventtype, i), Strength, Vitality, Agility, Intelligence, Luck, false, false, true));
            }

        }
    }
}
