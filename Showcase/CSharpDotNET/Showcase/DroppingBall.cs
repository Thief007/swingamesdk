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
        private static Sprite ball1;

        private static Vector GravityConstant = Physics.CreateVector(0, (Single)0.5);
        private static Single Movement = (Single)5.0;
        private static Vector AirResistanceV = Physics.CreateVector(0, (Single)0.2);
        private static Vector AirResistanceH = Physics.CreateVector((Single)(0.01),0);

        private static Matrix2D Rotate;

        private static bool falling = false;

        public static void Run()
        {
            Rotate = Physics.RotationMatrix((Single)180);

            ball1 = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            ball1.Movement.SetTo(Physics.CreateVector((Single)Movement, 0));
            ball1.Mass = 1;
       
            ball1.xPos = 40;
            ball1.yPos = 20;

            do
            {
                if (ball1.Movement.Y > 0 && !falling)
                {
                    //AirResistanceV = Physics.InvertVector(AirResistanceV);
                    AirResistanceV = Physics.Multiply(Rotate, AirResistanceV);
                    falling = true;
                }
                if ((ball1.Movement.X < (Single)0.0) && (AirResistanceH.X < (Single)0.0))
                {
                    //AirResistanceH = Physics.Multiply(Rotate, ref AirResistanceH);
                    AirResistanceH = Physics.InvertVector(AirResistanceH);
                    
                }
                if ((ball1.Movement.X > (Single)0.0) && (AirResistanceH.X > (Single)0.0))
                {
                    //AirResistanceH = Physics.Multiply(Rotate, ref AirResistanceH);
                    AirResistanceH = Physics.InvertVector(AirResistanceH);
                }

                ball1.Movement.SetTo(Physics.AddVectors(ball1.Movement, GravityConstant));
                ball1.Movement.SetTo(Physics.AddVectors(ball1.Movement, AirResistanceV));
                ball1.Movement.SetTo(Physics.AddVectors(ball1.Movement, AirResistanceH));

                MoveBallUsingVector(ref ball1);

                Graphics.DrawSprite(ball1);

                Core.ProcessEvents();
                Core.RefreshScreen();
                Graphics.ClearScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
        }

        public static void MoveBallUsingVector(ref Sprite ball)
        {
            Graphics.MoveSprite(ball, ball.Movement);

            if (ball.xPos > (Core.ScreenWidth() - Graphics.CurrentWidth(ball)))
            {
                ball.Movement.X = (Single)(ball.Movement.X * -1);
                ball.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball);
            }

            if (ball.yPos > Core.ScreenHeight() - Graphics.CurrentHeight(ball))
            {
                if (ball.Movement.Y < 1)
                {
                    ball.Movement.Y = 0;
                }

                ball.Movement.Y = (Single)(ball.Movement.Y * -1);
                ball.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball) - 1;

                AirResistanceV = Physics.Multiply(Rotate, AirResistanceV);

                falling = false;
            }

            if (ball.xPos < 0)
            {
                ball.Movement.X = (Single)(ball.Movement.X * -1);
                ball.xPos = 0;
            }

            if (ball1.yPos < 0)
            {
                ball.Movement.Y = (Single)(ball.Movement.Y * -1);
                ball.yPos = 0;
            }

        }

    }
}
