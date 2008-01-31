using System;
using System.Collections.Generic;
using System.Text;
using SwinGameVB;
using Color = System.Drawing.Color;

namespace Tests
{
    class CameraTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Camera Tests");
            result.Add(new TestCamera());
            list.Add(result);
        }

        #endregion

        private class TestCamera : TestSet
        {
            private readonly static string METHS =
                "All camera routines";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the ship." + Environment.NewLine +
                "I: Follow the ship" + Environment.NewLine +
                "U: Unfollow" + Environment.NewLine +
                "W: Move up the visual area" + Environment.NewLine +
                "S: Move down the visual area" + Environment.NewLine +
                "A: Move the visual area to" + Environment.NewLine +
                "the left" + Environment.NewLine +
                "D: Move the visual area to" + Environment.NewLine +
                "the right";

			private SwinGameVB.Bitmap bgImage = GameResources.GameImage("Sea");
			private Sprite shipSprite = Consts.Graphics.CreateSprite_MultiFPC(GameResources.GameImage("Ship"), 3, 2, 40, 43);
			private bool follow = false;

            public TestCamera() : base(METHS, INST) 
            {
                shipSprite.SetX(100);
                shipSprite.SetY(  100);
            }


            protected override void ToRun(Rectangle drawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))  shipSprite.SetX(shipSprite.GetX() + 4);
		        if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))  shipSprite.SetY(shipSprite.GetY() + 4);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) shipSprite.SetY(  shipSprite.GetY() - 4);
		        if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))  shipSprite.SetX(shipSprite.GetX() - 4);
		        if (Consts.Input.IsKeyPressed(Keys.VK_U))  follow = false;
		        if (Consts.Input.IsKeyPressed(Keys.VK_I))  follow = true;
                if (Consts.Input.IsKeyPressed(Keys.VK_A)) Consts.Camera.MoveVisualArea_WithVecotr(Consts.Physics.CreateVector_NoInvert(-20, 0));
                if (Consts.Input.IsKeyPressed(Keys.VK_D)) Consts.Camera.MoveVisualArea_WithVecotr(Consts.Physics.CreateVector_NoInvert(20, 0));
                if (Consts.Input.IsKeyPressed(Keys.VK_W))Consts.Camera.MoveVisualArea(0, -20);
                if (Consts.Input.IsKeyPressed(Keys.VK_S))Consts.Camera.MoveVisualArea(0, 20);		
		        if (follow)Consts.Camera.FollowSprite(shipSprite, (int)(400 - drawIn.GetX() - drawIn.GetWidth() / 2), -50);
        		
		        Consts.Graphics.DrawBitmap(bgImage, 0, 0);
               Consts.Text.DrawText("0, 0", Color.Red.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
               Consts.Text.DrawTextOnScreen("Ship Position: " + Convert.ToString(Consts.Camera.GameX(Consts.Camera.ScreenX(shipSprite.GetX()))) + ", " + Convert.ToString(Consts.Camera.GameY(Consts.Camera.ScreenY(shipSprite.GetY()))), Color.Red.ToArgb(), GameResources.GameFont("Courier"), (int)(drawIn.GetX()), (int)(drawIn.GetY()));
               Consts.Text.DrawTextOnScreen("Ship Position On Screen: " + Convert.ToString(Consts.Camera.ScreenX(shipSprite.GetX())) + ", " + Convert.ToString(Consts.Camera.ScreenY(shipSprite.GetY())), Color.Red.ToArgb(), GameResources.GameFont("Courier"), (int)(drawIn.GetX()), (int)(drawIn.GetY()) + 15);
               Consts.Text.DrawTextOnScreen("Camera Offset: " + Convert.ToString(Consts.Camera.XOffset()) + ", " + Convert.ToString(Consts.Camera.YOffset()), Color.Red.ToArgb(), GameResources.GameFont("Courier"), (int)(drawIn.GetX()), (int)(drawIn.GetY()) + 30);


                Consts.Graphics.DrawSprite(shipSprite);
                Consts.Graphics.UpdateSprite(shipSprite);
            }
        }
    }
}
