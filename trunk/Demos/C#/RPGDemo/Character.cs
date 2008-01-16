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
    public struct Attibutes
    {
        public int Strength;
        public int Vitality;
        public int Agility;
    }

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

    public enum DamageType
    {
        Heal,
        Enemy,
        Player,
        Critical,
        Evade,
        None,
    }

    public struct Character
    {
        public Sprite Sprite;
        public CharacterAnim Anim;

        public Stats Stats;
        public Attibutes Attributes;

        public Boolean Alive;

        public Sprite SlashUp;
        public Sprite SlashDown;
        public Sprite SlashLeft;
        public Sprite SlashRight;
        public Sprite CurrentSlash;

        public SoundEffect Swing;

        public Boolean Attacking;
        public int Cooldown;

        public Boolean CanAttack;
        public Boolean CanMove;
        public Boolean CanInteract;

        public DamageType DamageType;
        public int Damage;
        public int StatusCooldown;

        public Item Item;
        public bool HasItem;
    }

    public static class Characters
    {
        public static Character NewCharacter(String name, int SpawnX, int SpawnY, int Strength, int Vitality, int Agility, Boolean canMove, Boolean canAttack, Boolean canInteract)
        {
            Character temp = new Character();
            temp.Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);
            temp.Sprite.xPos = SpawnX;
            temp.Sprite.yPos = SpawnY;

            temp.Sprite.EndingAction = SpriteEndingAction.ReverseLoop;
            temp.Sprite.Movement.SetTo(Physics.CreateVector(0, 0));
            temp.Anim = CharacterAnim.None;

            temp.Attributes.Strength = Strength;
            temp.Attributes.Vitality = Vitality;
            temp.Attributes.Agility = Agility;

            RefreshCharacterStats(ref temp);

            temp.Stats.Health = temp.Stats.MaxHealth;
            temp.Stats.StatPoints = 8;
            temp.Stats.Level = 1;
            temp.Stats.Experience = 0;
            temp.Stats.ExperienceNextLevel = 100;

            temp.SlashUp = Graphics.CreateSprite(Resources.GameImage("Slash Up"), 1, 5, 28, 47);
            temp.SlashDown = Graphics.CreateSprite(Resources.GameImage("Slash Down"), 1, 5, 27, 47);
            temp.SlashLeft = Graphics.CreateSprite(Resources.GameImage("Slash Left"), 1, 5, 49, 27);
            temp.SlashRight = Graphics.CreateSprite(Resources.GameImage("Slash Right"), 1, 5, 49, 27);

            temp.SlashUp.EndingAction = SpriteEndingAction.Stop;
            temp.SlashDown.EndingAction = SpriteEndingAction.Stop;
            temp.SlashLeft.EndingAction = SpriteEndingAction.Stop;
            temp.SlashRight.EndingAction = SpriteEndingAction.Stop;

            temp.CurrentSlash = temp.SlashUp;

            temp.Attacking = false;

            temp.Swing = Resources.GameSound("Swing");

            temp.CanMove = canMove;
            temp.CanAttack = canAttack;
            temp.CanInteract = canInteract;

            temp.Sprite.UsePixelCollision = false;
            temp.Alive = true;

            temp.Damage = 0;
            temp.DamageType = DamageType.None;
            temp.StatusCooldown = 0;

            temp.HasItem = false;

            return temp;
        }

        public static void InitiateAttack(ref Character theCharacter)
        {
            if (!theCharacter.Attacking && theCharacter.Cooldown == 0)
            {
                //In each case, the Current Weapon is changed to the weapon that
                //represents the direction the character is facing
                //The sword's Animation is replayed
                //The Characters Attacking State is set to true
                //A Cooldown is set to down the character from spamming the attack
                //Sound Effect is played
                switch (theCharacter.Anim)
                {
                    case CharacterAnim.Top:
                        theCharacter.CurrentSlash = theCharacter.SlashUp;
                        Graphics.ReplayAnimation(theCharacter.CurrentSlash);
                        theCharacter.Attacking = true;
                        theCharacter.Cooldown = 40;
                        Audio.PlaySoundEffect(theCharacter.Swing);
                        break;

                    case CharacterAnim.Down:
                        theCharacter.CurrentSlash = theCharacter.SlashDown;
                        Graphics.ReplayAnimation(theCharacter.CurrentSlash);
                        theCharacter.Attacking = true;
                        theCharacter.Cooldown = 40;
                        Audio.PlaySoundEffect(theCharacter.Swing);
                        break;

                    case CharacterAnim.Left:
                        theCharacter.CurrentSlash = theCharacter.SlashLeft;
                        Graphics.ReplayAnimation(theCharacter.CurrentSlash);
                        theCharacter.Attacking = true;
                        theCharacter.Cooldown = 40;
                        Audio.PlaySoundEffect(theCharacter.Swing);
                        break;

                    case CharacterAnim.Right:
                        theCharacter.CurrentSlash = theCharacter.SlashRight;
                        Graphics.ReplayAnimation(theCharacter.CurrentSlash);
                        theCharacter.Attacking = true;
                        theCharacter.Cooldown = 40;
                        Audio.PlaySoundEffect(theCharacter.Swing);
                        break;
                }
            }
        }

        public static void UpdateAttack(ref Character theCharacter)
        {
            //Reduces the Cooldown to 0 slowly
            if (theCharacter.Cooldown > 0)
                theCharacter.Cooldown = theCharacter.Cooldown - 1;

            if (theCharacter.Attacking)
            {
                //Remove when added to tutorial
                if (!theCharacter.CurrentSlash.hasEnded)
                {
                    //Update the position of the Sword
                    //Each direction has a different offset, so that the sword is placed in the correct
                    //position. When you do this for your game, its a matter of trial and error, until
                    //you get the position you want.
                    switch (theCharacter.Anim)
                    {
                        case CharacterAnim.Top:
                            theCharacter.CurrentSlash.xPos = theCharacter.Sprite.xPos - (int)(theCharacter.Sprite.Width / 2);
                            theCharacter.CurrentSlash.yPos = theCharacter.Sprite.yPos - (int)(theCharacter.Sprite.Height / 2) + 6;
                            break;

                        case CharacterAnim.Down:
                            theCharacter.CurrentSlash.xPos = theCharacter.Sprite.xPos - (int)(theCharacter.Sprite.Width / 2);
                            theCharacter.CurrentSlash.yPos = theCharacter.Sprite.yPos +(int)(theCharacter.Sprite.Height / 2) - 13;
                            break;

                        case CharacterAnim.Left:
                            theCharacter.CurrentSlash.xPos = theCharacter.Sprite.xPos - (int)(theCharacter.Sprite.Width / 2);
                            theCharacter.CurrentSlash.yPos = theCharacter.Sprite.yPos;
                            break;

                        case CharacterAnim.Right:
                            theCharacter.CurrentSlash.xPos = theCharacter.Sprite.xPos - (int)(theCharacter.Sprite.Width / 2);
                            theCharacter.CurrentSlash.yPos = theCharacter.Sprite.yPos;
                            break;
                    }

                    //Update Sprite Animation and Draw the Sword Sprite
                    Graphics.UpdateSpriteAnimation(theCharacter.CurrentSlash);
                    Graphics.DrawSprite(theCharacter.CurrentSlash);
                }
                else
                {
                    //Set the Attacking state to false
                    theCharacter.Attacking = false;
                }
            }
        }

        

        public static void AddStat(ref Character theCharacter, String stat)
        {
            if (theCharacter.Stats.StatPoints > 0)
            {
                switch (stat)
                {
                    case "Strength":
                        //Add Strength
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Strength = theCharacter.Attributes.Strength + 1;
                        Core.Sleep(100);
                        break;
                    case "Agility":
                        //Add Agility
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Agility = theCharacter.Attributes.Agility + 1;
                        Core.Sleep(100);
                        break;
                    case "Vitality":
                        //Add Vitality
                        theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints - 1;
                        theCharacter.Attributes.Vitality = theCharacter.Attributes.Vitality + 1;
                        Core.Sleep(100);
                        break;
                }
            }
        }

        public static void RefreshCharacterStats(ref Character theCharacter)
        {
            //Health = Base(20) + Vitality * 10
            theCharacter.Stats.MaxHealth = 20 + theCharacter.Attributes.Vitality * 10;

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
                theCharacter.Stats.StatPoints = theCharacter.Stats.StatPoints + 5;

                //Give the Character his health back
                theCharacter.Stats.Health = theCharacter.Stats.MaxHealth;
            }
            
            //Attack = Base(5) + (Stength)
            theCharacter.Stats.Attack = 5 + (theCharacter.Attributes.Strength);

            //Defense = Base(1) + Vitality
            theCharacter.Stats.Defense = 1 + theCharacter.Attributes.Vitality;

            //Evasion = Base(1%) + (Agility / 2)
            theCharacter.Stats.Evasion = (int)(1 + (theCharacter.Attributes.Agility/2));

            //Critical Rate = Base(1%) + (Agility / 3)
            theCharacter.Stats.CriticalRate = Math.Round(1 + (double)(theCharacter.Attributes.Agility / 3));

        }

        public static void MoveCharacter(ref Character theCharacter, Map theMap, int moveX, int moveY)
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

            UpdateAttack(ref theCharacter);
        }

        public static void SetAnimationFrames(Sprite theSprite, int startingFrame, int startingIndex, int endingIndex)
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

        public static void DamageCharacter(ref Character theCharacter, int healthamount, DamageType type)
        {
            //Damage Character
            theCharacter.Stats.Health = theCharacter.Stats.Health - healthamount;

            //Set the damage change
            theCharacter.Damage = healthamount;

            //Set the damage type
            theCharacter.DamageType = type;

            //Set the status change cooldown
            theCharacter.StatusCooldown = 60;
        } 

        public static void HealCharacter(ref Character theCharacter, int healthamount)
        {
            //Heal Character
            theCharacter.Stats.Health = theCharacter.Stats.Health + healthamount;
            
            //Make sure his health isn't over the maximum
            if (theCharacter.Stats.Health > theCharacter.Stats.MaxHealth)
            {
                //Set the health to the characters maximum health
                theCharacter.Stats.Health = theCharacter.Stats.MaxHealth;
            }

            //Set the damage change
            theCharacter.Damage = healthamount;

            //Set the damage type
            theCharacter.DamageType = DamageType.Heal;

            //Set the status change cooldown
            theCharacter.StatusCooldown = 60;

            //Play the Healing Sound effect
            Audio.PlaySoundEffect(Resources.GameSound("Heal"));
        }

        public static void UpdateCharacterStatus(ref Character theCharacter)
        {
            //if the characters status change cooldown is above 0
            if (theCharacter.StatusCooldown > 0)
            {
                //Check which type of damage is being dealty
                switch (theCharacter.DamageType)
                {
                    //Healing
                    case DamageType.Heal:
                        Text.DrawText(Convert.ToString(theCharacter.Damage),
                            Color.LightGreen, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 80 + theCharacter.StatusCooldown);
                        break;
             
                    //Enemy Damage
                    case DamageType.Enemy:
                        Text.DrawText(Convert.ToString(theCharacter.Damage),
                            Color.Red, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 80 + theCharacter.StatusCooldown);
                        break;

                    //Player Damage
                    case DamageType.Player:
                        Text.DrawText(Convert.ToString(theCharacter.Damage),
                            Color.White, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 80 + theCharacter.StatusCooldown);
                        break;

                    //Evade an attack
                    case DamageType.Evade:
                        Text.DrawText("Evaded",
                            Color.Yellow, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 80 + theCharacter.StatusCooldown);
                        break;

                    //Critical Hit
                    case DamageType.Critical:
                        Text.DrawText("Critical Hit!",
                            Color.Orange, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 110 + theCharacter.StatusCooldown);
                        Text.DrawText(Convert.ToString(theCharacter.Damage),
                            Color.Orange, Resources.GameFont("Arial"),
                            (int)theCharacter.Sprite.xPos, (int)theCharacter.Sprite.yPos - 80 + theCharacter.StatusCooldown);
                        break;
                }

                //Reduce status cooldown by 1
                theCharacter.StatusCooldown = theCharacter.StatusCooldown - 1;
            }
        }
    }
}
