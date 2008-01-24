using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using SwinGame;

namespace Tests
{
    class FontTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Font and Text Tests");
            result.Add(new DrawText());

            list.Add(result);
        }

        #endregion

        private class DrawText : TestSet
        {
            private readonly static string METHS =
                "DrawText, DrawTextOnScreen, DrawTextLines, DrawTextLinesOnScreen, GetColor,";

            private readonly static string INST =
                "[C]hange Color";

            private static Color _TextColor = Color.White;
            private static int _x = 255;
            private static int _y = 255;
            private static int _z = 255;

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 200);

            public DrawText() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_C))
                {
                    _x = _x + 1;
                    _y = _y + 1;
                    _z = _z + 1;

                    if (_x > 255) _x = 0;
                    if (_x < 0) _x = 255;
                    if (_y > 255) _y = 0;
                    if (_y < 0) _y = 255;
                    if (_z > 255) _z = 0;
                    if (_z < 0) _z = 255;

                   _TextColor = Core.GetColor((byte)_x, (byte)(_y*_x), (byte)(_z*_x*_y));
                }

                //Draw Text
                Text.DrawText("This is some Text", _TextColor, GameResources.GameFont("Courier"), 10, 10);
                Text.DrawText("This is some Text", _TextColor, GameResources.GameFont("Courier"), Shapes.CreatePoint(10, 30));
                
                //Draw Text On Bitmap
                SwinGame.Graphics.ClearSurface(draw, Color.Black);
                Text.DrawText(draw, "This is some Text", _TextColor, GameResources.GameFont("Courier"), Shapes.CreatePoint(0, 0));
                SwinGame.Graphics.DrawBitmap(draw, 10, 50);
                SwinGame.Graphics.ClearSurface(draw, Color.Black);
                Text.DrawText(draw, "This is some Text", _TextColor, GameResources.GameFont("Courier"), 0, 0);
                SwinGame.Graphics.DrawBitmap(draw, 10, 70);

                //Draw Text On Screen
                Text.DrawTextOnScreen("This is some Text", _TextColor, GameResources.GameFont("Courier"), 32, 220);
                Text.DrawTextOnScreen("This is some Text", _TextColor, GameResources.GameFont("Courier"), Shapes.CreatePoint(32, 240));
                
                //Draw Text Lines
                Text.DrawTextLines("This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 10, 130, 100, 100);
                Text.DrawTextLines("This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Shapes.CreateRectangle(10, 170, 100, 100));

                //Draw Text Lines on Bitmap
                SwinGame.Graphics.ClearSurface(draw, Color.Black);
                Text.DrawTextLines(draw, "This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Shapes.CreateRectangle(0, 0, 100, 100));
                SwinGame.Graphics.DrawBitmap(draw, 10, 210);
                SwinGame.Graphics.ClearSurface(draw, Color.Black);
                Text.DrawTextLines(draw, "This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 0, 0, 100, 100);
                SwinGame.Graphics.DrawBitmap(draw, 10, 250);

                //Draw Text Lines on Screen
                Text.DrawTextLinesOnScreen("This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 32, 440, 100, 100);
                Text.DrawTextLinesOnScreen("This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Shapes.CreateRectangle(32, 480, 100, 100));
            }
        }
    }
}
