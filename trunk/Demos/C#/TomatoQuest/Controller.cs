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
using Keys = SwinGame.Keys;

using GameResources;

namespace TomatoQuest
{
    public class Controller
    {
        //Update Input Routine
        public void UpdateInput(Character thePlayer, Level theLevel)     
        {
            //If Up key is hit, move the character up
            if (Input.IsKeyPressed(SwinGame.Keys.VK_UP))
            {
                thePlayer.MoveCharacter(theLevel.Map, 0, -2);
            }
            //If Down key is hit, Move the character down
            else if (Input.IsKeyPressed(SwinGame.Keys.VK_DOWN))
            {
                thePlayer.MoveCharacter(theLevel.Map, 0, 2);
            }         
            //If Left key is hit, move the character left
            else if (Input.IsKeyPressed(SwinGame.Keys.VK_LEFT))
            {
                thePlayer.MoveCharacter(theLevel.Map, -2, 0);
            }
            //If Right key is hit, move the character right
            else if (Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT))
            {
                thePlayer.MoveCharacter(theLevel.Map, 2, 0);
            }
            //If no directional key is hit, don't move him
            else
            {
                thePlayer.MoveCharacter(theLevel.Map, 0, 0);
            }

            //ADD THESE LINES
            if (Input.IsKeyPressed(SwinGame.Keys.VK_SPACE))
            {
                thePlayer.InitiateAttack();
            }

            //Update Character's Animation
            thePlayer.UpdateCharacterAnimation();
        }
    }
}


      

  