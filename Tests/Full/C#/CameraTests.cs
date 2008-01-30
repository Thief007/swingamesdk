using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Drawing;

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

			private SwinGame.Bitmap bgImage = GameResources.GameImage("Sea");
			private Sprite shipSprite = SwinGame.Graphics.CreateSprite(GameResources.GameImage("Ship"), 3, 2, 40, 43);
			private bool follow = false;

            public TestCamera() : base(METHS, INST) 
            {
                shipSprite.X = 100; 
                shipSprite.Y = 100;
            }


            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_RIGHT))  shipSprite.X = shipSprite.X + 4;
		        if (Input.IsKeyPressed(Keys.VK_DOWN))  shipSprite.Y = shipSprite.Y + 4;
		        if (Input.IsKeyPressed(Keys.VK_UP))  shipSprite.Y = shipSprite.Y - 4;
		        if (Input.IsKeyPressed(Keys.VK_LEFT))  shipSprite.X = shipSprite.X - 4;
		        if (Input.IsKeyPressed(Keys.VK_U))  follow = false;
		        if (Input.IsKeyPressed(Keys.VK_I))  follow = true;
		        if (Input.IsKeyPressed(Keys.VK_A))  Camera.MoveVisualArea(Physics.CreateVector(-20, 0));
                if (Input.IsKeyPressed(Keys.VK_D)) Camera.MoveVisualArea(Physics.CreateVector(20, 0));
                if (Input.IsKeyPressed(Keys.VK_W)) Camera.MoveVisualArea(0, -20);
                if (Input.IsKeyPressed(Keys.VK_S)) Camera.MoveVisualArea(0, 20);		
		        if (follow) Camera.FollowSprite(shipSprite, (int)(400 - drawIn.X - drawIn.Width / 2), -50);
        		
		        SwinGame.Graphics.DrawBitmap(bgImage, 0, 0);
                Text.DrawText("0, 0", Color.Red, GameResources.GameFont("Courier"), 0, 0);
                Text.DrawTextOnScreen("Ship Position: " + Convert.ToString(Camera.GameX(Camera.ScreenX(shipSprite.X))) + ", " + Convert.ToString(Camera.GameY(Camera.ScreenY(shipSprite.Y))), Color.Red, GameResources.GameFont("Courier"), (int)(drawIn.X), (int)(drawIn.Y));
                Text.DrawTextOnScreen("Ship Position On Screen: " + Convert.ToString(Camera.ScreenX(shipSprite.X)) + ", " + Convert.ToString(Camera.ScreenY(shipSprite.Y)), Color.Red, GameResources.GameFont("Courier"), (int)(drawIn.X), (int)(drawIn.Y) + 15);
                Text.DrawTextOnScreen("Camera Offset: " + Convert.ToString(Camera.XOffset) + ", " + Convert.ToString(Camera.YOffset), Color.Red, GameResources.GameFont("Courier"), (int)(drawIn.X), (int)(drawIn.Y) + 30);


                SwinGame.Graphics.DrawSprite(shipSprite);
                SwinGame.Graphics.UpdateSprite(shipSprite);
            }
        }
    }
}
