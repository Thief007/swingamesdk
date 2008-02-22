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
    public static class Combat
    {
        private static bool CheckCritical(Character attacker, Random rnd)
        {
            //Get a random number, representing 0 to 100%
            int roll = rnd.Next(100);

            //If the roll% is within the attacker critical rate %, 
            //the attacker scored a critical hit
            if (roll <= attacker.CriticalRate)
            {
                return true;
            }

            return false;
        }

        private static bool CheckEvasion(Character defender, Random rnd)
        {
            //Get a random number, representing 0 to 100%
            int roll = rnd.Next(100);

            //If the roll% is within the defender's evasion %, the defender has evaded the attack
            if (roll <= defender.Evasion)
            {
                return true;
            }

            return false;
        }

        public static void AIHitPlayer(Character thePlayer, List<Character> theAI, Random randomnumber)
        {
            //Go through each AI
            for (int i = 0; i < theAI.Count; i++)
            {
                //Check that the AI is alive, On screen, and is attacking
                if (theAI[i].Alive && !Graphics.IsSpriteOffscreen(theAI[i].Sprite) && theAI[i].Attacking)
                {
                    //Check that the AI's attack animation is at the final frame
                    if (theAI[i].CurrentSlash.CurrentFrame == theAI[i].CurrentSlash.FramesPerCell.Length - 1)
                    {
                        //Check that the AI and Player is within striking distance
                        if (AIController.CalculateDistance(thePlayer, theAI[i]) < 100)
                        {
                            //Check that the AI sword sprite has collided with the player
                            if (Physics.HaveSpritesCollided(thePlayer.Sprite, theAI[i].CurrentSlash))
                            {
                                //Check if Player evaded
                                if (CheckEvasion(thePlayer, randomnumber))
                                {
                                    thePlayer.DamageCharacter(0, DamageType.Evade);
                                }
                                //Check if AI score a Critical Hit
                                else if (CheckEvasion(theAI[i], randomnumber))
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Critical"));
                                    thePlayer.DamageCharacter(theAI[i].Attack * 3, DamageType.Critical);
                                }
                                //Ordinary Hit
                                else
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Hit"));
                                    thePlayer.DamageCharacter((theAI[i].Attack * (100 - thePlayer.Defense)) / 100, DamageType.Enemy);
                                }

                                //Set the attacking state to false
                                theAI[i].Attacking = false;
                            }
                        }
                    }
                }
            }
        }

        public static void PlayerHitAI(Character thePlayer, List<Character> theAI, Random randomnumber)
        {
            //If the player's attack animation is at the last frame, and is attacking
            if (thePlayer.CurrentSlash.CurrentFrame == thePlayer.CurrentSlash.FramesPerCell.Length - 1 && thePlayer.Attacking)
            {
                //for each AI
                for (int i = 0; i < theAI.Count; i++)
                {
                    //if AI, is alive, onscreen
                    if (theAI[i].Alive && !Graphics.IsSpriteOffscreen(theAI[i].Sprite))
                    {
                        //if the AI is within attacking distance
                        if (AIController.CalculateDistance(thePlayer, theAI[i]) < 100)
                        {
                            //If the player's sword has hit the AI
                            if (Physics.HaveSpritesCollided(theAI[i].Sprite, thePlayer.CurrentSlash))
                            {
                                //Check if AI evaded
                                if (CheckEvasion(theAI[i], randomnumber))
                                {
                                    theAI[i].DamageCharacter(0, DamageType.Evade);
                                }
                                //Check if Player score a Critical Hit
                                else if (CheckCritical(thePlayer, randomnumber))
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Critical"));
                                    theAI[i].DamageCharacter(thePlayer.Attack * 3, DamageType.Critical);
                                }
                                //If its a normal Hit
                                else
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Hit"));
                                    theAI[i].DamageCharacter((thePlayer.Attack * (100 - theAI[i].Defense)) / 100, DamageType.Player);
                                }

                                //If the AI's health is less then 0, the AI has died
                                //and so, the Player gains experience
                                if (theAI[i].Health <= 0)
                                {
                                    thePlayer.Experience = thePlayer.Experience + theAI[i].Experience;
                                }

                                //Set Players Attacking to false
                                thePlayer.Attacking = false;
                            }
                        }
                    }
                }
            }
        }
    }
}
