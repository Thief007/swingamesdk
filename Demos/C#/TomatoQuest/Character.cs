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
    public enum CharacterAnim
    {
        Top,
        Down,
        Left,
        Right,
        None
    }


    public class Character
    {
        //Constant
        const Event PLAYERSPAWN = Event.Event1;

        private Sprite _Sprite;
        private CharacterAnim _Anim;

        //Attributes
        public int Strength;
        public int Vitality;
        public int Agility;
        public int Intelligence;
        public int Luck;

        //Stats
        public int Attack;
        public int MagicAttack;
        public int Defense;
        public int Evasion;
        public int AttackSpeed;
        public Double CriticalRate;
        public int Health;
        public int MaxHealth;
        public int Mana;
        public int MaxMana;
        public int StatPoints;
        public int SkillPoints;
        public int Level;
        public int Experience;
        public int ExperienceNextLevel;

        //Returns the Sprite
        public Sprite Sprite
        {
            get { return _Sprite; }
        }

        //Returns the Animation
        public CharacterAnim Anim
        {
            get { return _Anim; }
        }

        //Character Constructor
        public Character(String name, int SpawnX, int SpawnY, int strength, int vitality, int agility, int intelligence, int luck)
        {
            //Load the Character Sprite
            _Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);

            //Position the Character
            _Sprite.xPos = SpawnX;
            _Sprite.yPos = SpawnY;

            _Sprite.EndingAction = SpriteEndingAction.ReverseLoop;
            _Sprite.Movement.SetTo(Physics.CreateVector(0, 0));
            _Anim = CharacterAnim.None;

            Strength = strength;
            Vitality = vitality;
            Agility = agility;
            Intelligence = intelligence;
            Luck = luck;

            this.RefreshCharacterStats();

            Health = MaxHealth;
            Mana = MaxMana;
            StatPoints = 8;
            SkillPoints = 0;
            Level = 1;
            Experience = 0;
            ExperienceNextLevel = 100;
        }

        public void DrawCharacter()
        {
            //Draw the Character to the Screen
            Graphics.DrawSprite(_Sprite);
        }

        public void MoveCharacter(Map theMap, int moveX, int moveY)
        {
            //Create a Vector that represents the Sprites new movement
            Vector tempVector = Physics.CreateVector((float)moveX, (float)moveY);
            _Sprite.Movement.SetTo(tempVector);

            //Move Sprite to new Location
            Graphics.MoveSprite(_Sprite, _Sprite.Movement);

            //If Colliding with map, undo the movement.
            if (MappyLoader.CollisionWithMap(theMap, _Sprite) != CollisionSide.None)
            {
                Graphics.MoveSprite(_Sprite, Physics.InvertVector(_Sprite.Movement));
            }
        }

        private void SetAnimationFrames(int startingFrame, int startingIndex, int endingIndex)
        {
            //Create a new temporary Array, the size of the Frame Count
            int[] tempintarr = new int[_Sprite.FrameCount];

            //Set all elements to 0
            for (int i = 0; i < tempintarr.Length; i++)
            {
                tempintarr[i] = 0;
            }

            //Sets the Current Frame
            _Sprite.CurrentFrame = startingFrame;

            //Set the new Frame Values
            for (int i = startingIndex; i < endingIndex + 1; i++)
            {
                tempintarr[i] = 7;
            }

            //Set the Frames per cell to the new array
            _Sprite.FramesPerCell = tempintarr;
        }

        public void UpdateCharacterAnimation()
        {
            //If the Sprite Movement is going Up, set the animation Up
            if (_Sprite.Movement.Y < 0)
            {
                if (_Anim != CharacterAnim.Top)
                {
                    _Anim = CharacterAnim.Top;
                    SetAnimationFrames(1, 0, 2);
                }
                Graphics.UpdateSpriteAnimation(_Sprite);
            }

            //If the Sprite Movement is going Down, set the animation Down
            if (_Sprite.Movement.Y > 0)
            {
                if (_Anim != CharacterAnim.Down)
                {
                    _Anim = CharacterAnim.Down;
                    SetAnimationFrames(7, 6, 8);
                }
                Graphics.UpdateSpriteAnimation(_Sprite);
            }

            //If the Sprite Movement is going Left, set the animation Left
            if (_Sprite.Movement.X < 0)
            {
                if (_Anim != CharacterAnim.Left)
                {
                    _Anim = CharacterAnim.Left;
                    SetAnimationFrames(10, 9, 11);
                }
                Graphics.UpdateSpriteAnimation(_Sprite);
            }

            //If the Sprite Movement is going Right, set the animation Right
            if (_Sprite.Movement.X > 0)
            {
                if (_Anim != CharacterAnim.Right)
                {
                    _Anim = CharacterAnim.Right;
                    SetAnimationFrames(4, 3, 5);
                }
                Graphics.UpdateSpriteAnimation(_Sprite);
            }
        }

        public void AddAttribute(String attributeToAdd)
        {
            //If the Character has Stat Points
            if (StatPoints > 0)
            {
                switch (attributeToAdd)
                {
                    case "Strength":
                        //Subract a stat point, Add Strength
                        StatPoints--;
                        Strength++;
                        break;

                    case "Agility":
                        //Subract a stat point, Add Agility
                        StatPoints--;
                        Agility++;
                        break;

                    case "Vitality":
                        //Subract a stat point, Add Vitality
                        StatPoints--;
                        Vitality++;
                        break;

                    case "Intelligence":
                        //Subract a stat point, Add Intelligence
                        StatPoints--; 
                        Intelligence++;
                        break;

                    case "Luck":
                        //Subract a stat point, Add Luck
                        StatPoints--; 
                        Luck++;
                        break;
                }
            }
        }

        public void RefreshCharacterStats()
        {
            //Health = Base(20) + Vitality * 10
            MaxHealth = 20 + (Vitality * 10);

            //If characters current health is over the new max health, reduce character health
            if (Health > MaxHealth)
            {
                Health = MaxHealth;
            }

            //Mana = Base(10) + Intelligence * 5
            MaxMana = 10 + (Intelligence * 5);

            //If characters current mana is over the new max mana, reduce character mana
            if (Mana > MaxMana)
            {
                Mana = MaxMana;
            }

            //if the players experience is equal or greater then the next experience level, character gains a new level.
            if (Experience >= ExperienceNextLevel)
            {
                //Increase level by 1
                Level++;

                //Find the remaining experience to carry over to the next level, and set characters experience to it.
                Experience = Experience - ExperienceNextLevel;

                //Increase the amount of experience needed to get the next level
                ExperienceNextLevel = (int)(ExperienceNextLevel * 1.5);

                //Give the Character some stat Points
                StatPoints = StatPoints + 5;

                //Give the Character a skill point
                SkillPoints++;
            }

            //Attack = Base(5) + Stength
            Attack = 5 + Strength;

            //Magic Attack = Base(5) + Intelligence
            MagicAttack = 5 + Intelligence;

            //Defense = Base(1) + Vitality
            Defense = 1 + Vitality;

            //Evasion = Base(1%) + (Agility / 2)
            Evasion = (int)(1 + (Agility / 2));

            //The lower the Attack Speed, the faster the attack
            //Attack Speed = Base(60) - (Agility)
            AttackSpeed = 60 - Agility;

            //Attack Speed must be at least 1.
            if (AttackSpeed <= 0)
            {
                AttackSpeed = 1;
            }

            //Critical Rate = Base(1%) + (Luck)
            CriticalRate = 1 + Luck;
        }
    }
}

