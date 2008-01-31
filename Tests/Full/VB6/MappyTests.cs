using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using SwinGameVB;

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

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public MapCollisionTest() : base(METHS, INST) { }

            private Map _Map = GameResources.GameMap("test3");
            private Sprite _Ball = Consts.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));

            private int _TileX = 0;
            private int _TileY = 0;

            protected override void ToRun(Rectangle toDrawIn)
            {
               Consts.MappyLoader.DrawMap(_Map);

               _Ball.SetMovementVector((Consts.Physics.CreateVector_NoInvert(0, 0)));

                //Move Rectangle
               if (Consts.Input.IsKeyPressed(Keys.VK_UP)) _Ball.SetMovementVector(Consts.Physics.CreateVector_NoInvert(-0, -1)); ;
               if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) _Ball.SetMovementVector(Consts.Physics.CreateVector_NoInvert(0, 1)); ;
               if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) _Ball.SetMovementVector(Consts.Physics.CreateVector_NoInvert(-1, 0)); ;
               if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) _Ball.SetMovementVector(Consts.Physics.CreateVector_NoInvert(1, 0)); ;

                Consts.Graphics.MoveSprite_NoVector(_Ball);
                Consts.Graphics.DrawSprite(_Ball);

                if (Consts.MappyLoader.SpriteHasCollidedWithMapTile(_Map, _Ball, out _TileX, out _TileY))
                {
                    Consts.Graphics.DrawRectangle_Rectangle(Color.Red.ToArgb(),Consts.Shapes.CreateRectangle_Sprite(_Ball));
                    Consts.Graphics.DrawRectangle_Rectangle(Color.Red.ToArgb(),Consts.Shapes.CreateRectangle(150, 150, 50, 50));

                    switch (Consts.MappyLoader.WillCollideOnSide(_Map, _Ball))
                    {
                        case CollisionSide.Left:
                            Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), Consts.Shapes.CreateLine(_Ball.GetX() - 10, _Ball.GetY() - 10, _Ball.GetX() - 10, _Ball.GetY() + _Ball.GetHeight() + 10));
                            break;
                        case CollisionSide.Right:
                            Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), Consts.Shapes.CreateLine(_Ball.GetX() + 10 + Consts.Graphics.CurrentWidth(_Ball), _Ball.GetY() - 10, _Ball.GetX() + 10 + _Ball.GetWidth(), _Ball.GetY() + _Ball.GetHeight() + 10));
                            break;
                        case CollisionSide.Top:
                            Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), Consts.Shapes.CreateLine(_Ball.GetX() - 10, _Ball.GetY() - 10, _Ball.GetX() + Consts.Graphics.CurrentWidth(_Ball) + 10, _Ball.GetY() - 10));
                            break;
                        case CollisionSide.Bottom:
                            Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), Consts.Shapes.CreateLine(_Ball.GetX() - 10, _Ball.GetY() + _Ball.GetHeight() + 10, _Ball.GetX() + _Ball.GetWidth() + 10, _Ball.GetY() + _Ball.GetHeight() + 10));
                            break;
                    }

                    if (Consts.Input.WasKeyTyped(Keys.VK_M))Consts.MappyLoader.MoveSpriteOutOfTile(_Map, _Ball, _TileX, _TileY);
                }
                else
                {
                    Consts.Graphics.DrawRectangle_Rectangle(Color.White.ToArgb(),Consts.Shapes.CreateRectangle_Sprite(_Ball));
                    Consts.Graphics.DrawRectangle_Rectangle(Color.White.ToArgb(),Consts.Shapes.CreateRectangle(150,150,50,50));
                }
            }
        }
    }
}
