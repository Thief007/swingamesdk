using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Sprite = SwinGame.Sprite;

namespace SGSDK_Showcase
{
    public static class DroppingBall
    {

        private static PhysicsData ball1 = new PhysicsData();

        private static Vector GravityConstant = Physics.CreateVector(0, (Single)0.5);
        private static Single Movement = (Single)5.0;
        private static Vector AirResistanceV = Physics.CreateVector(0, (Single)0.2);
        private static Vector AirResistanceH = Physics.CreateVector((Single)(0.015),0);

        private static Matrix2D Rotate;
        private static Matrix2D Rotate2;

        private static bool falling = false;

        public static void Run()
        {
            Rotate = Physics.RotationMatrix((Single)180);
            Rotate2 = Physics.RotationMatrix((Single)72);

            ball1.Sprite = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball_small.png", ResourceKind.ImageResource)));
            ball1.Movement = Physics.CreateVector((Single)Movement, 0);
            ball1.Mass = 1;
       
            ball1.Sprite.xPos = 40;
            ball1.Sprite.yPos = 20;

            for (int i = 0; i < 2000; i++)
            {
                if (ball1.Movement.y > 0 && !falling)
                {
                    AirResistanceV = Physics.InvertVector(AirResistanceV);
                    //AirResistanceV = Physics.Multiply(Rotate, ref AirResistanceV);
                    falling = true;
                }
                if ((ball1.Movement.x < (Single)0.0) && (AirResistanceH.x < (Single)0.0))
                {
                    //AirResistanceH = Physics.Multiply(Rotate, ref AirResistanceH);
                    AirResistanceH = Physics.InvertVector(AirResistanceH);
                    
                }
                if ((ball1.Movement.x > (Single)0.0) && (AirResistanceH.x > (Single)0.0))
                {
                    //AirResistanceH = Physics.Multiply(Rotate, ref AirResistanceH);
                    AirResistanceH = Physics.InvertVector(AirResistanceH);
                }

                ball1.Movement = Physics.AddVectors(ball1.Movement, GravityConstant);
                ball1.Movement = Physics.AddVectors(ball1.Movement, AirResistanceV);
                ball1.Movement = Physics.AddVectors(ball1.Movement, AirResistanceH);

                MoveBallUsingVector(ref ball1);

                Graphics.DrawSprite(ball1.Sprite);

                Core.ProcessEvents();
                Core.RefreshScreen();
                Graphics.ClearScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            }
        }

        public static void MoveBallUsingVector(ref PhysicsData ball)
        {
            Graphics.MoveSprite(ball.Sprite, ball.Movement);

            if (ball.Sprite.xPos > Core.ScreenWidth() - Graphics.CurrentWidth(ball.Sprite))
            {
                ball.Movement.x = (Single)(ball.Movement.x * -1);
                ball.Sprite.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball.Sprite);
            }

            if (ball.Sprite.yPos > Core.ScreenHeight() - Graphics.CurrentHeight(ball.Sprite))
            {
                if (ball.Movement.y < 1)
                {
                    ball.Movement.y = 0;
                }

                ball.Movement.y = (Single)(ball.Movement.y * -1);
                ball.Sprite.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball.Sprite);

                AirResistanceV = Physics.InvertVector(AirResistanceV);
                //AirResistanceV = Physics.Multiply(Rotate, ref AirResistanceV);

                falling = false;
            }


            if (ball.Sprite.xPos < 0)
            {
                ball.Movement.x = (Single)(ball.Movement.x * -1);
                ball.Sprite.xPos = 0;
            }

            if (ball.Sprite.yPos < 0)
            {        
                ball.Movement.y = (Single)(ball.Movement.y * -1);
                ball.Sprite.yPos = 0;
            }
        }

    }
}
