using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using CollisionSide = SwinGame.CollisionSide;
using Event = SwinGame.Event;


namespace SGSDK_Showcase
{
    public static class MapExample
    {
        private static Font _Font = GameResources.GameFont("Courier");
        private static Map _Map = new Map();
        private static Sprite[] _Balls;
        private static CollisionSide _CollisionSide;
        private static Vector _Gravity;

        public static void Run()
        {
            Graphics.ClearScreen();

            _Map = GameResources.GameMap("test");
            Array.Resize(ref _Balls, 2);

            for (int i = 0; i < 2; i++)
            {
                _Balls[i] = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
                _Balls[i].Movement.SetTo(Physics.CreateVector(Randoms.GetRandomNumber(15),0));
                _Balls[i].xPos = MappyLoader.EventPositionX(_Map, Event.Event1, i);
                _Balls[i].yPos = MappyLoader.EventPositionY(_Map, Event.Event1, i);
                _Balls[i].Mass = 1;
            }

            _Gravity = Physics.CreateVector((float)0, (float)0.7);

            do
            {
                if (Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT))
                {
                    Camera.MoveVisualArea(2, 0);
                }
                
                if (Input.IsKeyPressed(SwinGame.Keys.VK_DOWN))
                {
                    Camera.MoveVisualArea(0, 2);
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_UP))
                {
                    Camera.MoveVisualArea(0, -2);
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_LEFT))
                {
                    Camera.MoveVisualArea(-2, 0);
                }

                MappyLoader.Drawmap(_Map);

                for (int i = 0; i < 1; i++)
                {
                    for (int j = i + 1; j < 2; j++)
                    {
                        if (i != j)
                        {
                            if (Physics.HaveSpritesCollided(_Balls[i], _Balls[j]))
                            {
                                Physics.VectorCollision(_Balls[i], _Balls[j]);
                            }
                        }
                    }
                }

                for (int i = 0; i < 2; i++)
                {
                    _Balls[i].Movement.SetTo(Physics.AddVectors(_Balls[i].Movement, _Gravity));
                    _Balls[i].Movement.SetTo(Physics.MultiplyVector(_Balls[i].Movement, (float)0.995));
                    Graphics.MoveSprite(_Balls[i], _Balls[i].Movement);
                }

                for (int i = 0; i < 2; i++)
                {
                    _CollisionSide = MappyLoader.CollisionWithMap(_Map, _Balls[i]);

                    if ((_CollisionSide == CollisionSide.Right) || (_CollisionSide == CollisionSide.Left) || (_CollisionSide == CollisionSide.TopLeft) || (_CollisionSide == CollisionSide.TopRight) || (_CollisionSide == CollisionSide.BottomLeft) || (_CollisionSide == CollisionSide.BottomRight))
                    {
                        _Balls[i].Movement.X = _Balls[i].Movement.X * -1;
                    }

                    if ((_CollisionSide == CollisionSide.Top) || (_CollisionSide == CollisionSide.Bottom) || (_CollisionSide == CollisionSide.TopLeft) || (_CollisionSide == CollisionSide.TopRight) || (_CollisionSide == CollisionSide.BottomLeft) || (_CollisionSide == CollisionSide.BottomRight))
                    {
                        _Balls[i].Movement.Y = _Balls[i].Movement.Y * -1;
                    }
                }

                for (int i = 0; i < 2; i++)
                {
                    Graphics.DrawSprite(_Balls[i]);
                }

                Overlay.DrawOverlay("MappyLoader Example");
                Text.DrawFramerate(0,0,_Font);
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
    }
}
