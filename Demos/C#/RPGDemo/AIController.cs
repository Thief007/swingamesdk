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
    public static class AIController
    {
        private static void PointAI(ref Character theAI, Character thePlayer)
        {
            //If the player is above the AI, point the AI upward
            if (theAI.Sprite.yPos > thePlayer.Sprite.yPos)
            {
                Characters.SetAnimationFrames(theAI.Sprite, 1, 0, 2);
            }
            //If the player is below the AI, point the AI down
            else if (theAI.Sprite.yPos < thePlayer.Sprite.yPos)
            {
                Characters.SetAnimationFrames(theAI.Sprite, 7, 6, 8);
            }
            //If the player is to the left the AI, point the AI to the left
            else if (theAI.Sprite.xPos > thePlayer.Sprite.xPos)
            {
                Characters.SetAnimationFrames(theAI.Sprite, 10, 9, 11);
            }
            //If the player is to the right the AI, point the AI to the right
            else if (theAI.Sprite.xPos < thePlayer.Sprite.xPos)
            {
                Characters.SetAnimationFrames(theAI.Sprite, 4, 3, 5);
            }           
        }

        private static void MoveAI(ref Character theAI, Character thePlayer, Map theMap)
        {
            //If the player is above the AI, move the AI upward
            if (theAI.Sprite.yPos > thePlayer.Sprite.yPos)
            {
                Characters.MoveCharacter(ref theAI, theMap, 0, -1);
            }
            //If the player is below the AI, move the AI down
            else if (theAI.Sprite.yPos < thePlayer.Sprite.yPos)
            {
                Characters.MoveCharacter(ref theAI, theMap, 0, 1);
            }
            //If the player is to the left the AI, move the AI to the left
            else if (theAI.Sprite.xPos > thePlayer.Sprite.xPos)
            {
                Characters.MoveCharacter(ref theAI, theMap, -1, 0);
            }
            //If the player is to the right the AI, move the AI to the right
            else if (theAI.Sprite.xPos < thePlayer.Sprite.xPos)
            {
                Characters.MoveCharacter(ref theAI, theMap, 1, 0);
            }
        }

        public static void UpdateAI(ref Character[] theAI, Character thePlayer, Map theMap)
        {
            for (int i = 0; i < theAI.Length; i++)
            {
                if (!Graphics.IsSpriteOffscreen(theAI[i].Sprite))
                {

                    //If the AI can move, it will move.
                    if (theAI[i].CanMove && CalculateDistance(theAI[i], thePlayer) < 200)
                    {
                        //Moves the AI
                        MoveAI(ref theAI[i], thePlayer, theMap);
                    }
                    //Otherwise the AI just points towards the player
                    else if (CalculateDistance(theAI[i], thePlayer) < 200)
                    {
                        //Points the AI towards the Player
                        PointAI(ref theAI[i], thePlayer);
                    }

                    if (theAI[i].CanAttack && CalculateDistance(theAI[i], thePlayer) < 30)
                    {
                        Characters.InitiateAttack(ref theAI[i]);
                    }

                    //Checks if the AI has collided with the player, and if so
                    //Moves the back
                    AICollideWithPlayer(ref theAI[i], thePlayer);

                    //Updates the AI's Walking Animation
                    Characters.UpdateCharacterAnimation(ref theAI[i]);

                    //Draws the AI
                    Graphics.DrawSprite(theAI[i].Sprite);
                }
            }
        }

        public static void PlayerCollideWithAI(ref Character thePlayer, Character[] theAI)
        {
            //Goes through all the AI
            for (int i = 0; i < theAI.Length; i++)
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

        public static void AICollideWithPlayer(ref Character theAI, Character thePlayer)
        {
            //Checks if the AI has collided with an Player
            if (Physics.HaveSpritesCollided(thePlayer.Sprite, theAI.Sprite))
            {
                //Moves the AI back 1 step
                Graphics.MoveSprite(theAI.Sprite, Physics.InvertVector(theAI.Sprite.Movement));
            }
        }

        public static int CalculateDistance(Character character1, Character character2)
        {
            double distancex = character1.Sprite.xPos - character2.Sprite.xPos;
            double distancey = character1.Sprite.yPos - character2.Sprite.yPos;

            return (int)Math.Sqrt(Math.Pow(distancex, 2) + Math.Pow(distancey, 2));
        }
    
    }
}
