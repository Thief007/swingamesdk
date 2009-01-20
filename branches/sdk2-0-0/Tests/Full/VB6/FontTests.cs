using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using SwinGameVB;

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

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public DrawText() : base(METHS, INST) { }

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_C))
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

                   _TextColor =Color.FromArgb( Consts.Core.GetColor((byte)_x, (byte)(_y*_x), (byte)(_z*_x*_y)));
                }

                //Draw Text
               Consts.Text.DrawText("1 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
               Consts.Text.DrawText_Point("2 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"),Consts.Shapes.CreatePoint(10, 30));
                
                //Draw Text On Bitmap
                Consts.Graphics.ClearSurface_Colour(draw, Color.Black.ToArgb());
               Consts.Text.DrawText_ToBitmap_Point(draw, "3 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"),Consts.Shapes.CreatePoint(0, 0));
                Consts.Graphics.DrawBitmap(draw, 10, 50);
                Consts.Graphics.ClearSurface_Colour(draw, Color.Gray.ToArgb());
               Consts.Text.DrawText_ToBitmap(draw, "4 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
                Consts.Graphics.DrawBitmap(draw, 10, 70);

                //Draw Text On Screen
                Consts.Text.DrawTextOnScreen("5 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"), 32, 220);
                Consts.Text.DrawTextOnScreen_Point("6 This is some Text", _TextColor.ToArgb(), GameResources.GameFont("Courier"), Consts.Shapes.CreatePoint(32, 240));
                
                //Draw Text Lines
                Consts.Text.DrawTextLines("7 This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Transparent.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 10, 130, 100, 100);
                Consts.Text.DrawTextLines_Rectangle("8 This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Green.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Consts.Shapes.CreateRectangle(10, 170, 100, 100));

                //Draw Text Lines on Bitmap
                Consts.Graphics.ClearSurface_Colour(draw, Color.Transparent.ToArgb());
                Consts.Text.DrawTextLines_ToBitmap_Rectangle(draw, "9 This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Gray.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Consts.Shapes.CreateRectangle(0, 0, 100, 100));
                Consts.Graphics.DrawBitmap(draw, 10, 210);
                Consts.Graphics.ClearSurface_Colour(draw, Color.Transparent.ToArgb());
                Consts.Text.DrawTextLines_ToBitmap(draw, "A This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Blue.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 0, 0, 100, 100);
                Consts.Graphics.DrawBitmap(draw, 10, 250);

                //Draw Text Lines on Screen
                Consts.Text.DrawTextLinesOnScreen("B This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Transparent.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 32, 440, 100, 100);
                Consts.Text.DrawTextLinesOnScreen_Rectangle("C This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor.ToArgb(), Color.Yellow.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignCenter, Consts.Shapes.CreateRectangle(32, 480, 100, 100));
            }
        }
    }
}
