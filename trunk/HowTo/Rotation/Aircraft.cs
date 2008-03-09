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

namespace GameProject
{
    public class Aircraft
    {
        private Sprite _Aircraft;   // The aircraft moving
        private Sprite _Shadow;     // The aircraft's shadow
        private int _Count;
        private bool _Manual;
        
        public Aircraft()
        {
            _Aircraft = Graphics.CreateSprite(GameResources.GameImage("Aircraft"), 120, 12, 80, 80);
            _Shadow = Graphics.CreateSprite(GameResources.GameImage("AircraftShadow"), 120, 12, 40, 40);
            _Aircraft.X = 610;
            _Aircraft.Y = 300;
            _Aircraft.Movement.SetTo(Physics.CreateVector(0, -1));

            _Count = 0;
            _Manual = false;
        }
        
        public bool Manual
        {
            get { return _Manual; }
            set { _Manual = value; }
        }

        /// Align the aircraft's shadow with the aircraft sprite
        public void AlignShadow()
        {
            _Shadow.X = _Aircraft.X + 30;
            _Shadow.Y = _Aircraft.Y + 80;
            _Shadow.CurrentFrame = _Aircraft.CurrentFrame;    
        }

        //Change the direction of the aircraft - select the correct frame
        // and alter the direction of its movement
        public void ChangeDirection(int angle)
        {
            if(angle != 0)
            {
                Matrix2D m = Physics.RotationMatrix(angle);
                
                _Aircraft.Movement.SetTo(Physics.Multiply(m, _Aircraft.Movement));

                if (angle == -30)
                {
                  _Aircraft.CurrentFrame = (_Aircraft.CurrentFrame + 1) % 12;
                }
                else 
                {
                  _Aircraft.CurrentFrame = (_Aircraft.CurrentFrame - 1) % 12;
                  if (_Aircraft.CurrentFrame < 0) _Aircraft.CurrentFrame = 11;
                }
            }    
        }
        
        public void Wrap()
        {
            // Wrap the sprite when offscreen
            if (_Aircraft.X < -_Aircraft.Width) 
                _Aircraft.X += GameLogic.SCREEN_WIDTH;
            if (_Aircraft.X > GameLogic.SCREEN_WIDTH + _Aircraft.Width)
                _Aircraft.X -= GameLogic.SCREEN_WIDTH;
                
            if (_Aircraft.Y < -_Aircraft.Height)
                _Aircraft.Y += GameLogic.SCREEN_HEIGHT;
            if (_Aircraft.Y > GameLogic.SCREEN_HEIGHT + _Aircraft.Height)
                _Aircraft.Y -= GameLogic.SCREEN_HEIGHT;
        }
        
        public void Draw()
        {
            int offsetX = 0;
            int offsetY = 0;

            if (_Aircraft.X < 0)
              offsetX = GameLogic.SCREEN_WIDTH;
            else if (_Aircraft.X + _Aircraft.Width > GameLogic.SCREEN_WIDTH)
              offsetX = -GameLogic.SCREEN_WIDTH;

            if (_Aircraft.Y < 0)
              offsetY = GameLogic.SCREEN_HEIGHT;
            else if (_Aircraft.Y + _Aircraft.Height > GameLogic.SCREEN_HEIGHT)
              offsetY = -GameLogic.SCREEN_HEIGHT;

            //Draw the sprite in its current position.
            Graphics.DrawSprite(_Shadow);
            Graphics.DrawSprite(_Aircraft);

            if ((offsetX != 0) || (offsetY != 0))
            {
                //Draw it offset
                Graphics.DrawSprite(_Aircraft, offsetX, offsetY);
                Graphics.DrawSprite(_Shadow, offsetX, offsetY);
            }
        }
        
        public void Update()
        {
            if(false == Manual) 
            {
                _Count++;
                if (_Count % 120 == 0)
                {
                    ChangeDirection(-30);
                    _Count = 0;
                }
            }
            Graphics.MoveSprite(_Aircraft);
            AlignShadow();

            Wrap();
        }
    }
}