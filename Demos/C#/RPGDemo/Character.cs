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
    //Remove this when added to tutorial
    public struct Attibutes
    {
        public int Strength;
        public int Vitality;
        public int Agility;
    }

    //Remove this when added to tutorial
    public struct Stats
    {
        public int Attack;
        public int Defense;
        public int Evasion;

        public Double CriticalRate;

        public int Health;
        public int MaxHealth;

        public int StatPoints;

        public int Level;
        public int Experience;
        public int ExperienceNextLevel;
    }

    public enum CharacterAnim
    {
        Top,
        Down,
        Left,
        Right,
        None
    }

    public struct Character
    {
        public Sprite Sprite;
        public CharacterAnim Anim;

        //Remove this when added to tutorial
        public Stats Stats;
        public Attibutes Attributes;

        //Remove this when added to tutorial
        public Boolean Alive;
    }

    public static class Characters
    {
        public static Character NewCharacter(String name, int SpawnX, int SpawnY, int Strength, int Vitality, int Agility)
        {
            Character temp = new Character();
            temp.Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);
            temp.Sprite.xPos = SpawnX;
            temp.Sprite.yPos = SpawnY;

            temp.Sprite.EndingAction = SpriteEndingAction.ReverseLoop;
            temp.Sprite.Movement.SetTo(Physics.CreateVector(0, 0));
            temp.Anim = CharacterAnim.None;

            //Remove this when added to tutorial
            temp.Attributes.Strength = Strength;
            temp.Attributes.Vitality = Vitality;
            temp.Attributes.Agility = Agility;

            //Remove this when added to tutorial
            RefreshCharacterStats(ref temp);

            //Remove this when added to tutorial
            temp.Stats.Health = temp.Stats.MaxHealth;
            temp.Stats.StatPoints = 20;
            temp.Stats.Level = 1;
            temp.Stats.Experience = 0;
            temp.Stats.ExperienceNextLevel = 100;

            return temp;
        }

        public static void AddStat(ref Character theCharacter, String stat)
        {
            if (theCharacter.Stats.StatPoints > 0)
            {
                switch (stat)
                {
                    case "Strength":
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Strength = theCharacter.Attributes.Strength + 1;
                        Core.Sleep(100);
                        break;
                    case "Agility":
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Agility = theCharacter.Attributes.Agility + 1;
                        Core.Sleep(100);
                        break;
                    case "Vitality":
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Vitality = theCharacter.Attributes.Vitality + 1;
                        Core.Sleep(100);
                        break;
                }
            }
        }

        //Remove this when added to tutorial
        public static void RefreshCharacterStats(ref Character theCharacter)
        {
            //Health = Base(20) + Vitality * 6
            theCharacter.Stats.MaxHealth = 20 + theCharacter.Attributes.Vitality * 6;

            //If characters current health is over the new max health, reduce character health
            if (theCharacter.Stats.Health > theCharacter.Stats.MaxHealth)
            {
                theCharacter.Stats.Health = theCharacter.Stats.MaxHealth;
            }

            //if the players experience is equal or greater then the next experience level, character gains a new level.
            if (theCharacter.Stats.Experience >= theCharacter.Stats.ExperienceNextLevel)
            {
                //Increase level by 1
                theCharacter.Stats.Level = theCharacter.Stats.Level + 1;

                //Find the remaining experience to carry over to the next level, and set characters experience to it.
                theCharacter.Stats.Experience = theCharacter.Stats.Experience - theCharacter.Stats.ExperienceNextLevel;
            
                //Increase the amount of experience needed to get the next level
                theCharacter.Stats.ExperienceNextLevel = (int)(theCharacter.Stats.ExperienceNextLevel * 1.5);
            
                //Give the Character some stat Points
                theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints + 4;
            }
            
            //Attack = Base(5) + (Stength * 2)
            theCharacter.Stats.Attack = 5 + (theCharacter.Attributes.Strength * 2);

            //Defense = Base(1) + Vitality
            theCharacter.Stats.Defense = 1 + theCharacter.Attributes.Vitality;

            //Evasion = Base(1%) + (Agility / 2)
            theCharacter.Stats.Evasion = (int)(1 + (theCharacter.Attributes.Agility/2));

            //Critical Rate = Base(1%) + (Agility / 3)
            theCharacter.Stats.CriticalRate = Math.Round(1 + (double)(theCharacter.Attributes.Agility / 3));

        }

        public static void MoveCharacter(Character theCharacter, Map theMap, int moveX, int moveY)
        {
            //Create a Vector that represents the Sprites new movement
            Vector tempVector = Physics.CreateVector((float)moveX, (float)moveY);
            theCharacter.Sprite.Movement.SetTo(tempVector);

            //Move Sprite to new Location
            Graphics.MoveSprite(theCharacter.Sprite, theCharacter.Sprite.Movement);

            //If Colliding with map, undo the movement.
            if (MappyLoader.CollisionWithMap(theMap, theCharacter.Sprite) != CollisionSide.None)
            {
                Graphics.MoveSprite(theCharacter.Sprite, Physics.InvertVector(theCharacter.Sprite.Movement));
            }
        }

        public static void UpdateCharacterAnimation(ref Character theCharacter)
        {
            if (theCharacter.Sprite.Movement.Y < 0)
            {
                if (theCharacter.Anim != CharacterAnim.Top)
                {
                    theCharacter.Anim = CharacterAnim.Top;
                    SetAnimationFrames(theCharacter.Sprite, 1, 0, 2);
                }
                Graphics.UpdateSpriteAnimation(theCharacter.Sprite);
            }

            if (theCharacter.Sprite.Movement.Y > 0)
            {
                if (theCharacter.Anim != CharacterAnim.Down)
                {
                    theCharacter.Anim = CharacterAnim.Down;
                    SetAnimationFrames(theCharacter.Sprite, 7, 6, 8);
                }
                Graphics.UpdateSpriteAnimation(theCharacter.Sprite);
            }

            if (theCharacter.Sprite.Movement.X < 0)
            {
                if (theCharacter.Anim != CharacterAnim.Left)
                {
                    theCharacter.Anim = CharacterAnim.Left;
                    SetAnimationFrames(theCharacter.Sprite, 10, 9, 11);
                }
                Graphics.UpdateSpriteAnimation(theCharacter.Sprite);
            }

            if (theCharacter.Sprite.Movement.X > 0)
            {
                if (theCharacter.Anim != CharacterAnim.Right)
                {
                    theCharacter.Anim = CharacterAnim.Right;
                    SetAnimationFrames(theCharacter.Sprite, 4, 3, 5);
                }
                Graphics.UpdateSpriteAnimation(theCharacter.Sprite);
            }
        }

        private static void SetAnimationFrames(Sprite theSprite, int startingFrame, int startingIndex, int endingIndex)
        {
            int[] tempintarr = new int[theSprite.FrameCount];

            for (int i = 0; i < tempintarr.Length; i++)
            {
                tempintarr[i] = 0;
            }

            theSprite.CurrentFrame = startingFrame;

            for (int i = startingIndex; i < endingIndex + 1; i++)
            {
                tempintarr[i] = 7;
            }

            theSprite.FramesPerCell = tempintarr;
        }
    }
}
