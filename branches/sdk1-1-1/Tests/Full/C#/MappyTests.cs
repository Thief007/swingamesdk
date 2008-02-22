using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using SwinGame;

namespace Tests
{
    class MappyTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Mappy Tests");
            result.Add(new MapCollisionTest());

            list.Add(result);
        }

        #endregion

        private class MapCollisionTest : TestSet
        {
            private readonly static string METHS =
                "Mappy Collisions";

            private readonly static string INST =
                "[Arrow Keys] Move Ball" + Environment.NewLine +
                "[M]ove Ball out of Map";

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public MapCollisionTest() : base(METHS, INST) { }

            private Map _Map = GameResources.GameMap("test3");
            private Sprite _Ball = SwinGame.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));

            private int _TileX = 0;
            private int _TileY = 0;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                MappyLoader.DrawMap(_Map);

                _Ball.Movement.SetTo(Physics.CreateVector(0, 0));

                //Move Rectangle
                if (Input.IsKeyPressed(Keys.VK_UP)) _Ball.Movement.SetTo(Physics.CreateVector(-0, -1)); ;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) _Ball.Movement.SetTo(Physics.CreateVector(0, 1)); ;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) _Ball.Movement.SetTo(Physics.CreateVector(-1, 0)); ;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) _Ball.Movement.SetTo(Physics.CreateVector(1, 0)); ;

                SwinGame.Graphics.MoveSprite(_Ball);
                SwinGame.Graphics.DrawSprite(_Ball);

                if (MappyLoader.SpriteHasCollidedWithMapTile(_Map, _Ball, out _TileX, out _TileY))
                {
                    SwinGame.Graphics.DrawRectangle(Color.Red, Shapes.CreateRectangle(_Ball));
                    SwinGame.Graphics.DrawRectangle(Color.Red, Shapes.CreateRectangle(150, 150, 50, 50));

                    switch (MappyLoader.WillCollideOnSide(_Map, _Ball))
                    {
                        case CollisionSide.Left: 
                            SwinGame.Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(_Ball.X - 10, _Ball.Y - 10, _Ball.X - 10, _Ball.Y + _Ball.Height + 10));
                            break;
                        case CollisionSide.Right:
                            SwinGame.Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(_Ball.X + 10 + _Ball.Width, _Ball.Y - 10, _Ball.X + 10 + _Ball.Width, _Ball.Y + _Ball.Height + 10));
                            break;
                        case CollisionSide.Top:
                            SwinGame.Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(_Ball.X - 10, _Ball.Y - 10, _Ball.X + _Ball.Width + 10, _Ball.Y - 10));
                            break;
                        case CollisionSide.Bottom:
                            SwinGame.Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(_Ball.X - 10, _Ball.Y + _Ball.Height + 10, _Ball.X + _Ball.Width + 10, _Ball.Y + _Ball.Height + 10));
                            break;
                    }

                    if (Input.WasKeyTyped(Keys.VK_M)) MappyLoader.MoveSpriteOutOfTile(_Map, _Ball, _TileX, _TileY);
                }
                else
                {
                    SwinGame.Graphics.DrawRectangle(Color.White, Shapes.CreateRectangle(_Ball));
                    SwinGame.Graphics.DrawRectangle(Color.White, Shapes.CreateRectangle(150,150,50,50));
                }
            }
        }
    }
}
