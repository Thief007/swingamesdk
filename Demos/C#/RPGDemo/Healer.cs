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

namespace GameProject
{

    public static class Healers
    {
        //Remove when added
        const Event HEALERSPAWN = Event.Event2;

        public static Character[] NewHealers(String name, Map map, int Strength, int Vitality, int Agility)
        {
            //Create a temporary array that is as big as the number of healers on the map.
            Character[] healers = new Character[MappyLoader.EventCount(map, HEALERSPAWN)];

            //Goes through each healer and creates a new character for each one
            //Placing each healer at it's spawn point.
            for (int i = 0; i < healers.Length; i++)
            {
                healers[i] = Characters.NewCharacter(name, MappyLoader.EventPositionX(map, HEALERSPAWN,i), MappyLoader.EventPositionY(map, HEALERSPAWN,i), Strength, Vitality, Agility, false, false, true);
            }

            return healers;
        }
    }
}
