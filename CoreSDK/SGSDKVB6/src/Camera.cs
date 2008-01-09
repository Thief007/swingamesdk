using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace SwinGameVB
{
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("3BE3481F-34BC-421a-9F0B-464B05188625")]
    [ComVisible(true)]
    public class Camera : ICamera
    {
        public void MoveVisualArea_WithVecotr(Vector v)
        {
            SwinGame.Camera.MoveVisualArea(v.result);
        }

        public void MoveVisualArea(float dx, float dy)
        {
            SwinGame.Camera.MoveVisualArea(dx, dy);
        }

        public void SetScreenOffset(float dx, float dy)
        {
            SwinGame.Camera.SetScreenOffset(dx, dy);
        }

        public int XOffset()
        {
            return SwinGame.Camera.XOffset();
        }
        public int YOffset()
        {
            return SwinGame.Camera.YOffset();
        }
        public int ScreenX(float x)
        {
            return SwinGame.Camera.ScreenX(x);
        }
        public int ScreenY(float y)
        {
            return SwinGame.Camera.ScreenY(y);
        }
        public float GameX(int x)
        {
            return SwinGame.Camera.GameX(x);
        }
        public float GameY(int y)
        {
            return SwinGame.Camera.GameY(y);
        }
        public Vector ToGameCoordinates(Vector screenVector)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Camera.ToGameCoordinates(screenVector.result);
            return vector;
        }
        public void FollowSprite(Sprite sprite, int xOffset, int yOffset)
        {
            SwinGame.Camera.FollowSprite(sprite.result, xOffset, yOffset);
        }

    }
    [Guid("5FEA3C94-A7BD-468b-A617-1C7004F711EC")]
    [ComVisible(true)]
    public interface ICamera
    {
        int XOffset();
        int YOffset();
        int ScreenX(float x);
        int ScreenY(float y);
        float GameX(int x);
        float GameY(int y);
        Vector ToGameCoordinates(Vector screenVector);
        void MoveVisualArea_WithVecotr(Vector v);
        void MoveVisualArea(float dx, float dy);
        void SetScreenOffset(float dx, float dy);
        void FollowSprite(Sprite sprite, int xOffset, int yOffset);
    }
}
