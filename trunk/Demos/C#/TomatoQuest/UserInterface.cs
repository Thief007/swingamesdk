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
using Keys = SwinGame.Keys;

using GameResources;

namespace TomatoQuest
{
    public class UserInterface
    {
        private bool _statPage = false;

        public void DrawHealthBar(Character theCharacter)
        {
            //Draws the Health Vial
            Graphics.DrawBitmapOnScreen(Resources.GameImage("HealthVial"), 10, 10);

            //Draws the Mana Vial
            Graphics.DrawBitmapOnScreen(Resources.GameImage("HealthVial"), 10, 30);

            //Gets the Percentage of Health Remaning
            int healthpercentage = (int)((100 * theCharacter.Health) / theCharacter.MaxHealth);

            //Gets the Percentage of Mana Remaning
            int manapercentage = (int)((100 * theCharacter.Mana) / theCharacter.MaxMana);

            //Draws the Health inside the Health Bar
            for (int i = 0; i < healthpercentage; i++)
            {
                Graphics.DrawBitmapOnScreen(Resources.GameImage("Health"), 15 + (2 * i), 13);
            }

            //Draws the Mana inside the Mana Bar
            for (int i = 0; i < manapercentage; i++)
            {
                Graphics.DrawBitmapOnScreen(Resources.GameImage("Mana"), 15 + (2 * i), 33);
            }

            //Draw the Health and Mana Amounts over the Health and Mana Bars
            Text.DrawTextOnScreen(Convert.ToString(theCharacter.Health + "/" + theCharacter.MaxHealth), Color.White, Resources.GameFont("SmallCourier"), 80, 12);
            Text.DrawTextOnScreen(Convert.ToString(theCharacter.Mana + "/" + theCharacter.MaxMana), Color.White, Resources.GameFont("SmallCourier"), 80, 32);
        }

        //Draws the Overlay of the User Interface
        public void DrawOverlay()
        {
            Graphics.DrawBitmapOnScreen(Resources.GameImage("Overlay"), 0, 0);
        }

        //Toggles the Stat Page On and Off
        private void ToggleStatPage()
        {
            _statPage = !_statPage;
        }

        //Draws the Stat Page (if the _statPage bool is true)
        private void DrawStatsPage(Character theCharacter)
        {
            if (_statPage)
            {
                DrawOverlay();

                Text.DrawTextOnScreen("Stats", Color.White, Resources.GameFont("Arial"), 250, 5);


                //Draw the Health and Mana Text
                Text.DrawTextOnScreen("Health: " + Convert.ToString(theCharacter.Health) + "/" + Convert.ToString(theCharacter.MaxHealth), Color.White, Resources.GameFont("Arial"), 20, 50);
                Text.DrawTextOnScreen("Mana: " + Convert.ToString(theCharacter.Mana) + "/" + Convert.ToString(theCharacter.MaxMana), Color.White, Resources.GameFont("Arial"), 20, 70);

                //Draw Level and Experience
                Text.DrawTextOnScreen("Level: " + Convert.ToString(theCharacter.Level), Color.White, Resources.GameFont("Arial"), 20, 110);
                Text.DrawTextOnScreen("Experience: " + Convert.ToString(theCharacter.Experience) + "/" + Convert.ToString(theCharacter.ExperienceNextLevel), Color.White, Resources.GameFont("Arial"), 20, 130);

                //Stat Points
                Text.DrawTextOnScreen("Stat Points Remaining: " + Convert.ToString(theCharacter.StatPoints), Color.White, Resources.GameFont("Arial"), 20, 170);

                //Attributes
                Text.DrawTextOnScreen("Strength: " + Convert.ToString(theCharacter.Strength), Color.White, Resources.GameFont("Arial"), 300, 50);
                Text.DrawTextOnScreen("Vitality: " + Convert.ToString(theCharacter.Vitality), Color.White, Resources.GameFont("Arial"), 300, 70);
                Text.DrawTextOnScreen("Agility: " + Convert.ToString(theCharacter.Agility), Color.White, Resources.GameFont("Arial"), 300, 90);
                Text.DrawTextOnScreen("Intelligence: " + Convert.ToString(theCharacter.Intelligence), Color.White, Resources.GameFont("Arial"), 300, 110);
                Text.DrawTextOnScreen("Luck: " + Convert.ToString(theCharacter.Luck), Color.White, Resources.GameFont("Arial"), 300, 130);

                //If character stat points, let him add them somewhere
                if (theCharacter.StatPoints > 0)
                {
                    //Draw +'s
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 470, 50);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 470, 70);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 470, 90);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 470, 110);
                    Text.DrawTextOnScreen("+", Color.White, Resources.GameFont("Arial"), 470, 130);

                    //Draw Boxes
                    Graphics.DrawRectangleOnScreen(Color.White, 467, 50, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 467, 70, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 467, 90, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 467, 110, 20, 20);
                    Graphics.DrawRectangleOnScreen(Color.White, 467, 130, 20, 20);

                    //Check if the user is clicking inside a box, and if so, add the stat point appropriately
                    if (Input.MouseWasClicked(MouseButton.LeftButton))
                    {
                        if (Input.GetMousePosition().x > 467 && Input.GetMousePosition().x < 487)
                        {
                            if (Input.GetMousePosition().y > 50 && Input.GetMousePosition().y < 70)
                            {
                                //Add Strength
                                theCharacter.AddAttribute("Strength");
                            }

                            if (Input.GetMousePosition().y > 70 && Input.GetMousePosition().y < 90)
                            {
                                //Add Vitality
                                theCharacter.AddAttribute("Vitality");
                            }

                            if (Input.GetMousePosition().y > 90 && Input.GetMousePosition().y < 110)
                            {
                                //Add Agility
                                theCharacter.AddAttribute("Agility");
                            }

                            if (Input.GetMousePosition().y > 110 && Input.GetMousePosition().y < 130)
                            {
                                //Add Agility
                                theCharacter.AddAttribute("Intelligence");
                            }

                            if (Input.GetMousePosition().y > 130 && Input.GetMousePosition().y < 150)
                            {
                                //Add Agility
                                theCharacter.AddAttribute("Luck");
                            }
                        }
                    }
                }

                //Stats
                Text.DrawTextOnScreen("Attack: " + Convert.ToString(theCharacter.Attack), Color.White, Resources.GameFont("Arial"), 20, 260);
                Text.DrawTextOnScreen("Magic Attack: " + Convert.ToString(theCharacter.MagicAttack), Color.White, Resources.GameFont("Arial"), 20, 280);
                Text.DrawTextOnScreen("Defense: " + Convert.ToString(theCharacter.Defense), Color.White, Resources.GameFont("Arial"), 20, 300);
                Text.DrawTextOnScreen("Evasion: " + Convert.ToString(theCharacter.Evasion) + "%", Color.White, Resources.GameFont("Arial"), 20, 320);
                Text.DrawTextOnScreen("Attack Speed: " + Convert.ToString(Math.Round((double)theCharacter.AttackSpeed/60, 2)) + " seconds delay", Color.White, Resources.GameFont("Arial"), 20, 340);
                Text.DrawTextOnScreen("Critical Rate: " + Convert.ToString(theCharacter.CriticalRate) + "%", Color.White, Resources.GameFont("Arial"), 20, 360);
            }
        }

        //Execute the User Interface
        public void RunUI(Character theCharacter)
        {
            //Toggles the stat page if the Space Bar is hit
            if (Input.WasKeyTyped(SwinGame.Keys.VK_S))
            {
                ToggleStatPage();
            }

            //Draws the Health Bar and the Stat Page
            DrawHealthBar(theCharacter);
            DrawStatsPage(theCharacter);
        }
    }
}
