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
            //Go through each AI
            for (int i = 0; i < theAI.Count; i++)
            {
                //If the AI is Onscreen and alive, contine.
                if (!Graphics.IsSpriteOffscreen(theAI[i].Sprite) && theAI[i].Alive)
                {

                    //If the AI can move, and is within sight distance, move
                    if (theAI[i].CanMove && CalculateDistance(theAI[i], thePlayer) < 300 && CalculateDistance(theAI[i], thePlayer) > 30)
                    {
                        //Moves the AI
                        MoveAI(theAI[i], thePlayer, theMap);
                        //Checks if the AI has collided with the player, and if so
                        //Moves the back

                        //Check for collisions with the Player
                        AICollideWithPlayer(theAI[i], thePlayer);

                        //Check for collisions with other AI
                        AICollideWithAI(theAI[i], theAI, i);
                    }
                    //Otherwise the AI just points towards the player
                    else if (CalculateDistance(theAI[i], thePlayer) < 300)
                    {
                        //Points the AI towards the Player
                        PointAI(theAI[i], thePlayer);
                    }

                    //If the AI can attack, and is within attacking distance, attack
                    if (theAI[i].CanAttack && CalculateDistance(theAI[i], thePlayer) < 45)
                    {
                        theAI[i].InitiateAttack();
                    }

                    //Updates the AI's Walking Animation
                    theAI[i].UpdateCharacterAnimation();

                    //Draws the AI
                    Graphics.DrawSprite(theAI[i].Sprite);
                }
                
                if (theAI[i].CharacterHasItem() && !theAI[i].Alive)
                {
                    theAI[i].DropItems();
                }
                
                theAI[i].UpdateCharacterStatus();
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
                if (!Graphics.IsSpriteOffscreen(theAI[i].Sprite) && theAI[i].Alive && CalculateDistance(thePlayer, theAI[i]) < 120)
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

        public static int CalculateDistance(Character character1, Character character2)
        {
            double distancex = character1.Sprite.xPos - character2.Sprite.xPos;
            double distancey = character1.Sprite.yPos - character2.Sprite.yPos;

            return (int)Math.Sqrt(Math.Pow(distancex, 2) + Math.Pow(distancey, 2));
        }

        public void AICollideWithAI(Character theAI, List<Character> allAI, int index)
        {
            //Go through all the AI
            for (int i = 0; i < allAI.Count; i++)
            {
                //Skip the AI that is checking the other AI
                if (i != index)
                {
                    //Check that the AI is alive, Onscreen and within distance
                    if (!Graphics.IsSpriteOffscreen(allAI[i].Sprite) && allAI[i].Alive && CalculateDistance(theAI, allAI[i]) < 120)
                    {
                        //Check if the Sprites have Collided
                        if (Physics.HaveSpritesCollided(theAI.Sprite, allAI[i].Sprite))
                        {
                            //Move the AI Back 1 Step
                            Graphics.MoveSprite(theAI.Sprite, Physics.InvertVector(theAI.Sprite.Movement));
                        }
                    }
                }
            }
        }
    }
}
