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
                "DrawText, DrawTextLines";

            private readonly static string INST =
                "[C]hance Color";

            private static Color _TextColor = Color.White;
            private static int _x = 255;
            private static int _y = 255;
            private static int _z = 255;

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(100, 100);

            public DrawText() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                /*{SoundEffect se = GameResources.GameSound("Shock");
                if (Input.WasKeyTyped(Keys.VK_A)) Audio.PlaySoundEffect(se);
                if (Input.WasKeyTyped(Keys.VK_I))
                    if (false == Audio.IsSoundEffectPlaying(se))
                        Audio.PlaySoundEffect(se);
                if (Input.WasKeyTyped(Keys.VK_S)) Audio.StopSoundEffect(se);
                if (Input.WasKeyTyped(Keys.VK_L)) Audio.PlaySoundEffect(se, -1);*/

                if (Input.IsKeyPressed(Keys.VK_C))
                {
                    _x = _x + 1;
                    _y = _y + 1;
                    _z = _z + 1;
                    _TextColor = Core.GetColor((byte)_x, (byte)(_y*_x), (byte)(_z*_x*_y));
                }

                Text.DrawText("This is some Text", _TextColor, GameResources.GameFont("Courier"), 100, 100);
                Text.DrawText("This is some Text", _TextColor, GameResources.GameFont("Courier"), Shapes.CreatePoint(100, 120));
                Text.DrawText(draw, "This is some Text", _TextColor, GameResources.GameFont("Courier"), Shapes.CreatePoint(0, 0));
                SwinGame.Graphics.DrawBitmap(draw, 100, 140);
                Text.DrawText(draw, "This is some Text", _TextColor, GameResources.GameFont("Courier"), 0, 0);
                SwinGame.Graphics.DrawBitmap(draw, 100, 240);
                SwinGame.Graphics.DrawRectangle(draw, Color.White, Shapes.CreateRectangle(10, 10, 50, 50));
                SwinGame.Graphics.DrawBitmap(draw, 100, 240);
                //Text.DrawTextLines("This is some Text Lines" + Environment.NewLine + "** This is some Text Lines **", _TextColor, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignCenter, 100, 140, 100, 100);

                if (_x > 255) _x = 0;
                if (_x < 0) _x = 255;
                if (_y > 255) _y = 0;
                if (_y < 0) _y = 255;
                if (_z > 255) _z = 0;
                if (_z < 0) _z = 255;
            }
        }
    }
}
