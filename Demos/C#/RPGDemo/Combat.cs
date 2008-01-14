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
            for (int i = 0; i < theAI.Length; i++)
            {
                if (theAI[i].Alive && !Graphics.IsSpriteOffscreen(theAI[i].Sprite) && theAI[i].Attacking)
                {
                    if (theAI[i].CurrentSlash.CurrentFrame == theAI[i].CurrentSlash.FramesPerCell.Length - 1)
                    {
                        if (AIController.CalculateDistance(thePlayer, theAI[i]) < 100)
                        {
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
                                    Characters.DamageCharacter(ref thePlayer, theAI[i].Stats.Attack * 3, DamageType.Critical);
                                }
                                else
                                {
                                    Characters.DamageCharacter(ref thePlayer, (theAI[i].Stats.Attack * (100 - thePlayer.Stats.Defense)) / 100, DamageType.Enemy);
                                }

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
                                    Characters.DamageCharacter(ref theAI[i], thePlayer.Stats.Attack * 3, DamageType.Critical);
                                }
                                else
                                {
                                    Characters.DamageCharacter(ref theAI[i], (thePlayer.Stats.Attack * (100 - theAI[i].Stats.Defense)) / 100, DamageType.Player);
                                }

                                if (theAI[i].Stats.Health <= 0)
                                {
                                    thePlayer.Stats.Experience = thePlayer.Stats.Experience + theAI[i].Stats.Experience;
                                }

                                thePlayer.Attacking = false;
                            }
                        }
                    }
                }
            }
        }
    }
}
