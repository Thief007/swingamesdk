using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using SwinGame;
using Bitmap = SwinGame.Bitmap;
using Graphics = SwinGame.Graphics;
using SoundEffect = SwinGame.SoundEffect;
using Font = SwinGame.Font;

namespace GameProject
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("here");
            Core.OpenGraphicsWindow("Hi", 800, 600);

            Bitmap hey = Graphics.LoadBitmap("SwinGameAni.png");
            //hey.pointer = IntPtr.Zero;

            Audio.OpenAudio();

            Font font = Text.LoadFont("barial.ttf",20);

            SoundEffect effect = Audio.LoadSoundEffect("SwinGameStart.ogg");

            while (Core.WindowCloseRequested() != true)
            {
                //FrameRate
                Text.DrawFramerate(0, 0, font);

                //Mouse Movement
                Vector hi = Input.GetMouseMovement();
                Text.DrawText("Mouse Movement: " + hi.X.ToString() + "," + hi.Y.ToString(), Color.Blue, font, 1, 25);

                //Mouse Position
                hi = Input.GetMousePosition();
                Text.DrawText("Mouse Position: " + hi.X.ToString() + "," + hi.Y.ToString(), Color.Blue, font, 1, 50);

                if (Input.IsMouseDown(MouseButton.LeftButton))
                {
                    Audio.PlaySoundEffect(effect);
                }

                Graphics.DrawBitmap(hey, 10, 10);

                Core.RefreshScreen();
                Graphics.ClearScreen();
                Core.ProcessEvents();
            }

            Audio.FreeSoundEffect(ref effect);
   
            Audio.CloseAudio();
        }
    }
}
