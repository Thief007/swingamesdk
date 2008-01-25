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
                "GetMousePosition, GetMouseMovement, HideMouse, ShowMouse, MoveMouse, WasKeyTyped";

            private readonly static string INST =
                "[H]ide Mouse" + Environment.NewLine +
                "[S]how Mouse" + Environment.NewLine +
                "Move Mouse to [C]enter";

            public InputTest() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {

                Vector MouseMovement = SwinGame.Input.GetMouseMovement();
                
                Text.DrawText("Mouse Position : " + Convert.ToString(SwinGame.Input.GetMousePosition().X) + ", " + Convert.ToString(SwinGame.Input.GetMousePosition().Y), Color.White, GameResources.GameFont("Courier"), 10, 10);
                Text.DrawText("Mouse Movement : " + Convert.ToString(MouseMovement.X) + ", " + Convert.ToString(MouseMovement.Y), Color.White, GameResources.GameFont("Courier"), 10, 30);

                if (SwinGame.Input.WasKeyTyped(Keys.VK_H)) SwinGame.Input.HideMouse();
                if (SwinGame.Input.WasKeyTyped(Keys.VK_S)) SwinGame.Input.ShowMouse();
                if (SwinGame.Input.IsKeyPressed(Keys.VK_C)) SwinGame.Input.MoveMouse(400, 300);
            }
        }
    }
}
