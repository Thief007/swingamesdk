using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Drawing;

namespace Tests
{
    class InputTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Keyboard and Mouse Input Tests");
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

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Vector MouseMovement = SwinGame.Input.GetMouseMovement();
                
                Text.DrawText("Mouse Position : " + Convert.ToString(SwinGame.Input.GetMousePosition().X) + ", " + Convert.ToString(SwinGame.Input.GetMousePosition().Y), Color.White, GameResources.GameFont("Courier"), 10, 10);
                Text.DrawText("Mouse Movement : " + Convert.ToString(MouseMovement.X) + ", " + Convert.ToString(MouseMovement.Y), Color.White, GameResources.GameFont("Courier"), 10, 30);

                if (SwinGame.Input.WasKeyTyped(Keys.VK_H)) SwinGame.Input.HideMouse();
                if (SwinGame.Input.WasKeyTyped(Keys.VK_S)) SwinGame.Input.ShowMouse();
                if (SwinGame.Input.IsKeyPressed(Keys.VK_C)) SwinGame.Input.MoveMouse(400, 300);

                SwinGame.Graphics.DrawLineOnScreen(Color.LightBlue, Shapes.CreateLine(Input.GetMousePosition().X, 0, Input.GetMousePosition().X, 600));
                SwinGame.Graphics.DrawLineOnScreen(Color.LightBlue, Shapes.CreateLine(0, Input.GetMousePosition().Y, 800,Input.GetMousePosition().Y));

                Text.DrawText("Is Left Mouse Button Down : " + Convert.ToString(SwinGame.Input.IsMouseDown(MouseButton.LeftButton)), Color.White, GameResources.GameFont("Courier"), 10, 50);
                Text.DrawText("Was Left Mouse Button Clicked : " + Convert.ToString(SwinGame.Input.MouseWasClicked(MouseButton.LeftButton)), Color.White, GameResources.GameFont("Courier"), 10, 70);
                Text.DrawText("Is Mouse Cursor Shown : " + Convert.ToString(SwinGame.Input.IsMouseShown()), Color.White, GameResources.GameFont("Courier"), 10, 90);
            }
        }
    }

    class KeyBoardInputTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Keyboard and Mouse Input Tests");
            result.Add(new KeyBoardInputTest());

            list.Add(result);
        }

        #endregion

        private class KeyBoardInputTest : TestSet
        {
            private readonly static string METHS =
                "All Keyboard";

            private readonly static string INST =
                "Hit the [A] Key";

            public KeyBoardInputTest() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Text.DrawText("A Key Down : " + Convert.ToString(Input.IsKeyPressed(Keys.VK_A)), Color.White, GameResources.GameFont("Courier"), 10, 10);
                Text.DrawText("A Key Up : " + Convert.ToString(!Input.IsKeyPressed(Keys.VK_A)), Color.White, GameResources.GameFont("Courier"), 10, 30);
                Text.DrawText("A Key Typed : " + Convert.ToString(Input.WasKeyTyped(Keys.VK_A)), Color.White, GameResources.GameFont("Courier"), 10, 50);

                Text.DrawText("Is Reading Text : " + Convert.ToString(Input.IsReadingText()), Color.White, GameResources.GameFont("Courier"), 10, 90);

                //Input.StartReadingText(Color.White, 10, GameResources.GameFont("Courier"), 10, 110);
                //Text.DrawText("You Entered : " + Convert.ToString(Input.TextReadAsASCII()), Color.White, GameResources.GameFont("Courier"), 10, 130);
            }
        }
    }
}
