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
    public static class Interaction
    {
        public static void InteractWithHealers(ref Character thePlayer, Character[] Healers)
        {
            //Goes through all Healers
            for (int i = 0; i < Healers.Length; i++)
            {
                //Checks that the Healer is alive, onscreen, and canInteract
                if (Healers[i].Alive && !Graphics.IsSpriteOffscreen(Healers[i].Sprite) && Healers[i].CanInteract)
                {
                    //Checks that the Healer is within healing distance, and the player has hit the enter key
                    if (AIController.CalculateDistance(thePlayer, Healers[i]) < 40 && Input.IsKeyPressed(SwinGame.Keys.VK_RETURN) && Healers[i].Cooldown == 0)
                    {
                        //Checks which way the player is facing, if the player is facing towards the
                        //Healer, the Player gets healed. The Healer's cooldown gets set to 180, since
                        //the healer doesn't have a weapon, we can use the cooldown to stop the player
                        //from spamming the Healer for health.
                        switch (thePlayer.Anim)
                        {
                            case CharacterAnim.Down:
                                if (thePlayer.Sprite.yPos + thePlayer.Sprite.Height <= Healers[i].Sprite.yPos)
                                {
                                    Characters.HealCharacter(ref thePlayer, 20);
                                    Healers[i].Cooldown = 180;
                                }
                                break;

                            case CharacterAnim.Top:
                                if (thePlayer.Sprite.yPos >= Healers[i].Sprite.yPos + Healers[i].Sprite.Height)
                                {
                                    Characters.HealCharacter(ref thePlayer, 20);
                                    Healers[i].Cooldown = 180;
                                }
                                break;

                            case CharacterAnim.Left:
                                if (thePlayer.Sprite.xPos >= Healers[i].Sprite.xPos + Healers[i].Sprite.Width)
                                {
                                    Characters.HealCharacter(ref thePlayer, 20);
                                    Healers[i].Cooldown = 180;
                                }
                                break;

                            case CharacterAnim.Right:
                                if (thePlayer.Sprite.xPos + thePlayer.Sprite.Width <= Healers[i].Sprite.xPos)
                                {
                                    Characters.HealCharacter(ref thePlayer, 20);
                                    Healers[i].Cooldown = 180;
                                }
                                break;
                        }
                    }
                }

                //If the Healer's Cooldown is greater then 0, reduce the cooldown by 1.
                if (Healers[i].Cooldown > 0)
                {
                    Healers[i].Cooldown = Healers[i].Cooldown - 1;
                }
            }
        }
    }
}
