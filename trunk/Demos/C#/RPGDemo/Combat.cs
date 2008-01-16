using System;
using System.Drawing;

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
    public static class Combat
    {
        private static bool CheckEvasion(Character defender, ref Random rnd)
        {
            //Get a random number, representing 0 to 100%
            int roll = rnd.Next(100);

            //If the roll% is within the defender's evasion %, the defender has evaded the attack
            if (roll <= defender.Stats.Evasion)
            {
                return true;
            }

            return false;
        }

        private static bool CheckCritical(Character attacker, ref Random rnd)
        {
            //Get a random number, representing 0 to 100%
            int roll = rnd.Next(100);

            //If the roll% is within the attacker critical rate %, 
            //the attacker scored a critical hit
            if (roll <= attacker.Stats.CriticalRate)
            {
                return true;
            }

            return false;
        }

        public static void AIHitPlayer(ref Character thePlayer, ref Character[] theAI, ref Random randomnumber)
        {
            //Go through each AI
            for (int i = 0; i < theAI.Length; i++)
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
                                if (CheckEvasion(thePlayer, ref randomnumber))
                                {
                                    Characters.DamageCharacter(ref thePlayer, 0, DamageType.Evade);
                                }
                                //Check if AI score a Critical Hit
                                else if (CheckEvasion(theAI[i], ref randomnumber))
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Critical"));
                                    Characters.DamageCharacter(ref thePlayer, theAI[i].Stats.Attack * 3, DamageType.Critical);
                                }
                                //Ordinary Hit
                                else
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Hit"));
                                    Characters.DamageCharacter(ref thePlayer, (theAI[i].Stats.Attack * (100 - thePlayer.Stats.Defense)) / 100, DamageType.Enemy);
                                }

                                //Set the attacking state to false
                                theAI[i].Attacking = false;
                            }
                        }
                    }
                }
            }
        }

        public static void PlayerHitAI(ref Character thePlayer, ref Character[] theAI, ref Random randomnumber)
        {
            //If the player's attack animation is at the last frame, and is attacking
            if (thePlayer.CurrentSlash.CurrentFrame == thePlayer.CurrentSlash.FramesPerCell.Length - 1 && thePlayer.Attacking)
            {
                //for each AI
                for (int i = 0; i < theAI.Length; i++)
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
                                if (CheckEvasion(theAI[i], ref randomnumber))
                                {
                                    Characters.DamageCharacter(ref theAI[i], 0, DamageType.Evade);
                                }
                                //Check if Player score a Critical Hit
                                else if (CheckCritical(thePlayer, ref randomnumber))
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Critical"));
                                    Characters.DamageCharacter(ref theAI[i], thePlayer.Stats.Attack * 3, DamageType.Critical);
                                }
                                //If its a normal Hit
                                else
                                {
                                    Audio.PlaySoundEffect(Resources.GameSound("Hit"));
                                    Characters.DamageCharacter(ref theAI[i], (thePlayer.Stats.Attack * (100 - theAI[i].Stats.Defense)) / 100, DamageType.Player);
                                }

                                //If the AI's health is less then 0, the AI has died
                                //and so, the Player gains experience
                                if (theAI[i].Stats.Health <= 0)
                                {
                                    thePlayer.Stats.Experience = thePlayer.Stats.Experience + theAI[i].Stats.Experience;
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
