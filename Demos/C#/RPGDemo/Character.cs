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
    }

    public static class Characters
    {
        public static Character NewCharacter(String name, int SpawnX, int SpawnY)
        {
            Character temp = new Character();
            temp.Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);
            temp.Sprite.xPos = SpawnX;
            temp.Sprite.yPos = SpawnY;
            temp.Sprite.EndingAction = SpriteEndingAction.ReverseLoop;
            temp.Sprite.Movement.SetTo(Physics.CreateVector(0, 0));
            temp.Anim = CharacterAnim.None;
            return temp;
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
