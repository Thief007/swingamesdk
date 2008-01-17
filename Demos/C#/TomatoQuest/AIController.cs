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
    public class AIController
    {
        public void UpdateAI(List<Character> theAI, Character thePlayer, Map theMap)
        {
            for (int i = 0; i < theAI.Count; i++)
            {
                if (!Graphics.IsSpriteOffscreen(theAI[i].Sprite))
                {

                    //If the AI can move, it will move.
                    if (theAI[i].CanMove)
                    {
                        //Moves the AI
                        MoveAI(theAI[i], thePlayer, theMap);
                    }
                    //Otherwise the AI just points towards the player
                    else
                    {
                        //Points the AI towards the Player
                        PointAI(theAI[i], thePlayer);
                    }

                    //Checks if the AI has collided with the player, and if so
                    //Moves the back
                    AICollideWithPlayer(theAI[i], thePlayer);

                    //Updates the AI's Walking Animation
                    theAI[i].UpdateCharacterAnimation();

                    //Draws the AI
                    Graphics.DrawSprite(theAI[i].Sprite);
                }
            }
        }

        private void PointAI(Character theAI, Character thePlayer)
        {
            //If the player is above the AI, point the AI upward
            if (theAI.Sprite.yPos > thePlayer.Sprite.yPos)
            {
                theAI.SetAnimationFrames(1, 0, 2);
            }
            //If the player is below the AI, point the AI down
            else if (theAI.Sprite.yPos < thePlayer.Sprite.yPos)
            {
                theAI.SetAnimationFrames(7, 6, 8);
            }
            //If the player is to the left the AI, point the AI to the left
            else if (theAI.Sprite.xPos > thePlayer.Sprite.xPos)
            {
                theAI.SetAnimationFrames(10, 9, 11);
            }
            //If the player is to the right the AI, point the AI to the right
            else if (theAI.Sprite.xPos < thePlayer.Sprite.xPos)
            {
                theAI.SetAnimationFrames(4, 3, 5);
            }
        }

        private void MoveAI(Character theAI, Character thePlayer, Map theMap)
        {
            //If the player is above the AI, move the AI upward
            if (theAI.Sprite.yPos > thePlayer.Sprite.yPos)
            {
                theAI.MoveCharacter(theMap, 0, -1);
            }
            //If the player is below the AI, move the AI down
            else if (theAI.Sprite.yPos < thePlayer.Sprite.yPos)
            {
                theAI.MoveCharacter(theMap, 0, 1);
            }
            //If the player is to the left the AI, move the AI to the left
            else if (theAI.Sprite.xPos > thePlayer.Sprite.xPos)
            {
                theAI.MoveCharacter(theMap, -1, 0);
            }
            //If the player is to the right the AI, move the AI to the right
            else if (theAI.Sprite.xPos < thePlayer.Sprite.xPos)
            {
                theAI.MoveCharacter(theMap, 1, 0);
            }
        }

        public void PlayerCollideWithAI(Character thePlayer, List<Character> theAI)
        {
            //Goes through all the AI
            for (int i = 0; i < theAI.Count; i++)
            {
                //Checks if the Player has collided with an AI
                if (Physics.HaveSpritesCollided(thePlayer.Sprite, theAI[i].Sprite))
                {
                    //Moves the Player back
                    Graphics.MoveSprite(thePlayer.Sprite, Physics.InvertVector(thePlayer.Sprite.Movement));
                    //We only want to move back once, to avoid glitches where
                    //we bounce between 2 AI.
                    return;
                }
            }
        }

        public void AICollideWithPlayer(Character theAI, Character thePlayer)
        {
            //Checks if the AI has collided with an Player
            if (Physics.HaveSpritesCollided(thePlayer.Sprite, theAI.Sprite))
            {
                //Moves the AI back 1 step
                Graphics.MoveSprite(theAI.Sprite, Physics.InvertVector(theAI.Sprite.Movement));
            }
        }
    }
}
