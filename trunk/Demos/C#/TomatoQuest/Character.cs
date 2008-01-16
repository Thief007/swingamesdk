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
        public Character(String name, int SpawnX, int SpawnY)
        {
            //Load the Character Sprite
            _Sprite = Graphics.CreateSprite(Resources.GameImage(name), 3, 12, 24, 32);

            //Position the Character
            _Sprite.xPos = SpawnX;
            _Sprite.yPos = SpawnY;

            _Sprite.EndingAction = SpriteEndingAction.ReverseLoop;
            _Sprite.Movement.SetTo(Physics.CreateVector(0, 0));
            _Anim = CharacterAnim.None;
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
    }
}

