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
    public static class Enemy
    {
        const Event ENEMY1SPAWN = Event.Event4;
        const Event ENEMY2SPAWN = Event.Event5;
        const Event BOSSSPAWN = Event.Event3;

        public static Character[] NewEnemies(String name, int Enemy, Map map, int Strength, int Vitality, int Agility, int Experience)
        {
            Event enemytype = ENEMY1SPAWN;

            switch (Enemy)
            {
                case 1:
                    enemytype = ENEMY1SPAWN;
                    break;
                case 2:
                    enemytype = ENEMY2SPAWN;
                    break;
                case 3:
                    enemytype = BOSSSPAWN;
                    break;
            }

            //Create a temporary array that is as big as the number of healers on the map.
            Character[] enemies = new Character[MappyLoader.EventCount(map, enemytype)];

            //Goes through each healer and creates a new character for each one
            //Placing each healer at it's spawn point.
            for (int i = 0; i < enemies.Length; i++)
            {
                enemies[i] = Characters.NewCharacter(name, MappyLoader.EventPositionX(map, enemytype, i), MappyLoader.EventPositionY(map, enemytype, i), Strength, Vitality, Agility, true, true, false);
                enemies[i].Stats.Experience = Experience;
            }

            return enemies;
        }
    }
}
