using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using SwinGame;

namespace Tests
{
    class GraphicsTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Audio Tests");
            result.Add(new BitmapTest());
            result.Add(new DrawPixelTest());
            list.Add(result);
        }

        #endregion

        private class BitmapTest : TestSet
        {
            private readonly static string METHS =
                "Bitmap Routines";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "The ship";

            public BitmapTest()
                : base(METHS, INST)
            {
                Array.Resize(ref frameB, 6);
                frameB[0] = GameResources.GameImage("Frame1");
                frameB[1] = GameResources.GameImage("Frame2");
                frameB[2] = GameResources.GameImage("Frame3");
                frameB[3] = GameResources.GameImage("Frame4");
                frameB[4] = GameResources.GameImage("Frame5");
                frameB[5] = GameResources.GameImage("Frame6");
            }

            private SwinGame.Bitmap[] frameB;
            private SwinGame.Bitmap ship = GameResources.GameImage("enShip");
            private Point2D framePos = Shapes.CreatePoint(0, 0);
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) framePos.X = framePos.X - 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) framePos.X = framePos.X + 2;
                if (Input.IsKeyPressed(Keys.VK_UP)) framePos.Y = framePos.Y + 2;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) framePos.Y = framePos.Y - 2;

                SwinGame.Graphics.ClearSurface(smallScreen, Color.Black);

                SwinGame.Graphics.DrawBitmap(smallScreen, frameB[0], Shapes.CreatePoint(0,0));
                SwinGame.Graphics.DrawBitmap(smallScreen, frameB[2], 278, 0);

                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 50, 100, 100), Shapes.CreatePoint(29, 50));
                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 50, 120, 100), 149, 50);
                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 50), 100, 100, 289, 50);
		        
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);

                SwinGame.Graphics.DrawBitmap(frameB[1], Shapes.CreatePoint(139, 0));
		        SwinGame.Graphics.DrawBitmap(frameB[3], 0, 209);

                SwinGame.Graphics.DrawBitmapOnScreen(frameB[4], Shapes.CreatePoint(139 + toDrawIn.X, 209 + toDrawIn.Y));
                SwinGame.Graphics.DrawBitmapOnScreen(frameB[5], (int)Math.Round((double)(278 + toDrawIn.X)), (int)Math.Round((double)(209 + toDrawIn.Y)));
		
		        SwinGame.Graphics.DrawBitmapPart(ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 159, 100, 100), 29, 159);
		        SwinGame.Graphics.DrawBitmapPart(ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 159, 120, 100), Shapes.CreatePoint(149, 159));
                SwinGame.Graphics.DrawBitmapPart(ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 159), 100, 100, 289, 159);

                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 268, 100, 100), (int)Math.Round((double)(29 + toDrawIn.X)), (int)Math.Round((double)(268 + toDrawIn.Y)));
                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 268, 120, 100), Shapes.CreatePoint(149 + toDrawIn.X, 268 + toDrawIn.Y));
                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 268), 100, 100, (int)Math.Round((double)(289 + toDrawIn.X)), (int)Math.Round((double)(268 + toDrawIn.Y)));
            }
        }

        private class DrawPixelTest : TestSet
        {
            private readonly static string METHS =
                "DrawPixel";

            private readonly static string INST =
                "Click and drag to draw with" + Environment.NewLine +
                "your mouse cursor." + Environment.NewLine +
                "1: Increment Blue" + Environment.NewLine +
                "2: Increment Green" + Environment.NewLine +
                "3: Inrrement Blue";

            public DrawPixelTest()
                : base(METHS, INST)
            {
                base.ClearScreen = false;
            }

            private Color _curCol = Color.Transparent;
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Boolean started = false;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (!started)
                {
                    started = !started;
                    SwinGame.Graphics.ClearScreen();
                }

                if (Input.IsKeyPressed(Keys.VK_Z))
                {
                    SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
                    SwinGame.Graphics.ClearScreen(Color.Transparent);
                    SwinGame.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BGA"), (int)(toDrawIn.X), (int)(toDrawIn.Y));
                }

                if (Input.IsKeyPressed(Keys.VK_1)) _curCol = Core.GetColor((byte)(_curCol.R + 5), _curCol.G, _curCol.B);
                if (Input.IsKeyPressed(Keys.VK_2)) _curCol = Core.GetColor(_curCol.R, (byte)(_curCol.G + 5), _curCol.B);
                if (Input.IsKeyPressed(Keys.VK_3)) _curCol = Core.GetColor(_curCol.R, _curCol.G, (byte)(_curCol.B + 5));

                SwinGame.Graphics.FillRectangle(Color.Black, 0, 0, 300, 45);

                Text.DrawText("Red   : " + Convert.ToString(_curCol.R), Color.Red, GameResources.GameFont("Courier"), 0, 0);
		        Text.DrawText("Green : " + Convert.ToString(_curCol.G), Color.Green, GameResources.GameFont("Courier"), 0, 15);
		        Text.DrawText("Blue  : " + Convert.ToString(_curCol.B), Color.Blue, GameResources.GameFont("Courier"), 0, 30);
		
                if (Input.IsMouseDown(MouseButton.LeftButton))
                {
                    SwinGame.Graphics.DrawPixel(smallScreen, _curCol, Shapes.CreatePoint(Input.GetMousePosition().X - toDrawIn.X - 10, Input.GetMousePosition().Y - toDrawIn.Y - 10)); 
                    SwinGame.Graphics.DrawPixel(smallScreen, _curCol, (int)(Input.GetMousePosition().X - toDrawIn.X - 10), (int)(Input.GetMousePosition().Y- toDrawIn.Y - 10));   
                
                    SwinGame.Graphics.DrawPixel(_curCol, Shapes.CreatePoint(Input.GetMousePosition().X - 10 - toDrawIn.X, Input.GetMousePosition().Y - toDrawIn.Y));
                    SwinGame.Graphics.DrawPixel(_curCol, (int)(Input.GetMousePosition().X - toDrawIn.X + 10), (int)(Input.GetMousePosition().Y - toDrawIn.Y));

                    SwinGame.Graphics.DrawPixelOnScreen(_curCol, Shapes.CreatePoint(Input.GetMousePosition().X - 10, Input.GetMousePosition().Y + 10));
                    SwinGame.Graphics.DrawPixelOnScreen(_curCol, (int)(Input.GetMousePosition().X + 10), (int)(Input.GetMousePosition().Y + 10));
                }

		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
            }
        }
    }
}