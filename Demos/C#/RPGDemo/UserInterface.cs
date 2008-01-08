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
    public static class UserInterface
    {
        private static Boolean _statPage = false;

        public static void RunUI(ref Character theCharacter)
        {
            if (Input.IsKeyPressed(SwinGame.Keys.VK_S))
            {
                ToggleStatPage();
                Core.Sleep(200);
            }

            DrawHealthBar(theCharacter);
            DrawStatsPage(ref theCharacter);
        }

        //Draws the Overlay of the User Interface
        private static void DrawOverlay()
        {
            Graphics.DrawBitmapOnScreen(Resources.GameImage("Overlay"), 0 ,0);
        }

        private static void DrawHealthBar(Character theCharacter)
        {
            Graphics.DrawBitmapOnScreen(Resources.GameImage("HealthVial"), 20, 20);

            double rawpercentage = (100 * theCharacter.Stats.Health) / theCharacter.Stats.MaxHealth;
            int percentage = (int)rawpercentage;

            for (int i = 0; i < percentage; i++)
            {
                Graphics.DrawBitmapOnScreen(Resources.GameImage("Health"), 25 + (2 * i), 25);
            }
        }

        private static void ToggleStatPage()
        {
            _statPage = !_statPage;
        }

        private static void DrawStatsPage(ref Character theCharacter)
        {
            if (_statPage)
            {
                DrawOverlay();

                Text.DrawTextOnScreen("Stats", Color.White, Resources.GameFont("Arial"), 375, 5);


                //If player has 50% or more health, health appears in green
                //If player has 20% to 50% health, it appears in yellow
                //Else player's health appears red.
                if (theCharacter.Stats.Health > (theCharacter.Stats.MaxHealth / 2))
                {
                    Text.DrawTextOnScreen("Health: " + Convert.ToString(theCharacter.Stats.Health) + "/" + Convert.ToString(theCharacter.Stats.MaxHealth), Color.LightGreen, Resources.GameFont("Arial"), 50, 50);
                }
                else if (theCharacter.Stats.Health > (theCharacter.Stats.MaxHealth / 5))
                {
                    Text.DrawTextOnScreen("Health: " + Convert.ToString(theCharacter.Stats.Health) + "/" + Convert.ToString(theCharacter.Stats.MaxHealth), Color.Yellow, Resources.GameFont("Arial"), 50, 50);
                }
                else
                {
                    Text.DrawTextOnScreen("Health: " + Convert.ToString(theCharacter.Stats.Health) + "/" + Convert.ToString(theCharacter.Stats.MaxHealth), Color.Red, Resources.GameFont("Arial"), 50, 50);
                }

                Text.DrawTextOnScreen("Level: " + Convert.ToString(theCharacter.Stats.Level), Color.White, Resources.GameFont("Arial"), 50, 90);
                Text.DrawTextOnScreen("Experience: " + Convert.ToString(theCharacter.Stats.Experience) + "/" + Convert.ToString(theCharacter.Stats.ExperienceNextLevel), Color.White, Resources.GameFont("Arial"), 50, 110);

                //Stat Points
                Text.DrawTextOnScreen("Stat Points Remaining: " + Convert.ToString(theCharacter.Stats.StatPoints), Color.White, Resources.GameFont("Arial"), 50, 150);

                //Attributes
                Text.DrawTextOnScreen("Strength: " + Convert.ToString(theCharacter.Attributes.Strength), Color.White, Resources.GameFont("Arial"), 400, 50);
                Text.DrawTextOnScreen("Vitality: " + Convert.ToString(theCharacter.Attributes.Vitality), Color.White, Resources.GameFont("Arial"), 400, 70);
                Text.DrawTextOnScreen("Agility: " + Convert.ToString(theCharacter.Attributes.Agility), Color.White, Resources.GameFont("Arial"), 400, 90);

                //If character stat points, let him add them somewhere
                if (theCharacter.Stats.StatPoints > 0)
                {
                    //Draw +'s
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 520, 50);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 520, 70);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 520, 90);

                    //Draw Boxes
                    Graphics.DrawRectangleOnScreen(Color.White, 517, 50, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 517, 70, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 517, 90, 20, 20);

                    //Check if the user is clicking inside a box, and if so, add the stat point appropriately
                    if (Input.IsMouseDown(MouseButton.LeftButton))
                    {
                        if (Input.GetMousePosition().x > 517 && Input.GetMousePosition().x < 537)
                        {
                            if (Input.GetMousePosition().y > 50 && Input.GetMousePosition().y < 70)
                            {
                                Characters.AddStat(ref theCharacter, "Strength");
                            }

                            if (Input.GetMousePosition().y > 70 && Input.GetMousePosition().y < 90)
                            {
                                Characters.AddStat(ref theCharacter, "Vitality");
                            }

                            if (Input.GetMousePosition().y > 90 && Input.GetMousePosition().y < 110)
                            {
                                Characters.AddStat(ref theCharacter, "Agility");
                            }
                        }
                    }
                }

                //Stats
                Text.DrawTextOnScreen("Attack: " + Convert.ToString(theCharacter.Stats.Attack), Color.White, Resources.GameFont("Arial"), 50, 240);
                Text.DrawTextOnScreen("Defense: " + Convert.ToString(theCharacter.Stats.Defense), Color.White, Resources.GameFont("Arial"), 50, 260);
                Text.DrawTextOnScreen("Evasion: " + Convert.ToString(theCharacter.Stats.Evasion) + "%", Color.White, Resources.GameFont("Arial"), 50, 280);
                Text.DrawTextOnScreen("Critical Rate: " + Convert.ToString(theCharacter.Stats.CriticalRate) + "%", Color.White, Resources.GameFont("Arial"), 50, 300);
            }
        }
    }
}
