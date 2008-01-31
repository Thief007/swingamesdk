using System;
using System.Collections.Generic;
using System.Text;
using SwinGameVB;
using Color = System.Drawing.Color;

namespace Tests
{
    class InputTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Keyboard and Mouse Consts.Input Tests");
            result.Add(new InputTest());

            list.Add(result);
        }

        #endregion

        private class InputTest : TestSet
        {
            private readonly static string METHS =
                "All Mouse";

            private readonly static string INST =
                "[H]ide Mouse" + Environment.NewLine +
                "[S]how Mouse" + Environment.NewLine +
                "Move Mouse to [C]enter"  + Environment.NewLine +
                "Click the Left Mouse Button";

            public InputTest() : base(METHS, INST) { }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Vector MouseMovement = Consts.Input.GetMouseMovement();
               
               Consts.Text.DrawText("Mouse Position : " + Convert.ToString(Consts.Input.GetMousePosition().GetX()) + ", " + Convert.ToString(Consts.Input.GetMousePosition().GetY()), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
               Consts.Text.DrawText("Mouse Movement : " + Convert.ToString(MouseMovement.getX()) + ", " + Convert.ToString(MouseMovement.getY()), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);

                if (Consts.Input.WasKeyTyped(Keys.VK_H)) Consts.Input.HideMouse();
                if (Consts.Input.WasKeyTyped(Keys.VK_S)) Consts.Input.ShowMouse();
                if (Consts.Input.IsKeyPressed(Keys.VK_C)) Consts.Input.MoveMouse(400, 300);

                Consts.Graphics.DrawLineOnScreen_Line(Color.LightBlue.ToArgb(),Consts.Shapes.CreateLine(Consts.Input.GetMousePosition().GetX(), 0, Consts.Input.GetMousePosition().GetX(), 600));
                Consts.Graphics.DrawLineOnScreen_Line(Color.LightBlue.ToArgb(), Consts.Shapes.CreateLine(0, Consts.Input.GetMousePosition().GetY(), 800, Consts.Input.GetMousePosition().GetY()));

               Consts.Text.DrawText("Is Left Mouse Button Down : " + Convert.ToString(Consts.Input.IsMouseDown(MouseButton.LeftButton)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);
               Consts.Text.DrawText("Was Left Mouse Button Clicked : " + Convert.ToString(Consts.Input.MouseWasClicked(MouseButton.LeftButton)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 70);
               Consts.Text.DrawText("Is Mouse Cursor Shown : " + Convert.ToString(Consts.Input.IsMouseShown()), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 90);
            }
        }
    }

    class KeyBoardInputTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Keyboard and Mouse Consts.Input Tests");
            result.Add(new KeyBoardInputTest());

            list.Add(result);
        }

        #endregion

        private class KeyBoardInputTest : TestSet
        {
            private readonly static string METHS =
                "All Keyboard";

            private readonly static string INST =
                "Hit the [A] Key" + Environment.NewLine +
                "[S]tart Reading Text";

            private string _EnteredText;

            public KeyBoardInputTest() : base(METHS, INST) { }

            protected override void ToRun(Rectangle toDrawIn)
            {
               Consts.Text.DrawText("A Key Down : " + Convert.ToString(Consts.Input.IsKeyPressed(Keys.VK_A)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
               Consts.Text.DrawText("A Key Up : " + Convert.ToString(!Consts.Input.IsKeyPressed(Keys.VK_A)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);
               Consts.Text.DrawText("A Key Typed : " + Convert.ToString(Consts.Input.WasKeyTyped(Keys.VK_A)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);

               Consts.Text.DrawText("Is Reading Text : " + Convert.ToString(Consts.Input.IsReadingText()), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 90);

                if (!Consts.Input.IsReadingText() && Consts.Input.WasKeyTyped(Keys.VK_S))
                {
                    Consts.Input.StartReadingText(Color.White.ToArgb(), 10, GameResources.GameFont("Courier"), 35, 240);
                }
                _EnteredText = Consts.Input.TextReadAsASCII();
               Consts.Text.DrawText("You Entered : " + _EnteredText, Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 130);
            }
        }
    }
}
