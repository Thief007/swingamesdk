using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace SGSDK_Showcase
{
    public static class Randoms
    {
        private static Random _Random = new Random(System.DateTime.Now.Millisecond);

        public static int GetRandomNumber(int max)
        {
            return (int)Math.Round((double)_Random.Next(max), 0);
        }

        public static Color GetRandomColor()
        {
            int number = (int)Math.Round((double)_Random.Next(10), 0);

            switch (number)
            {
                case 0: return Color.Blue;
                case 1: return Color.Red;
                case 2: return Color.Green;
                case 3: return Color.Yellow;
                case 4: return Color.Pink;
                case 5: return Color.Orange;
                case 6: return Color.White;
                case 7: return Color.LightBlue;
                case 8: return Color.LightGreen;
                case 9: return Color.Magenta;
                case 10: return Color.MediumPurple;
            }

            return Color.Black;
        }
    }
}
