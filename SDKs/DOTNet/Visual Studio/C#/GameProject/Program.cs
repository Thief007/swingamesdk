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

            //Bitmap hey = Graphics.LoadBitmap("SwinGameAni.png");
            //hey.pointer = IntPtr.Zero;

            Audio.OpenAudio();

            Font font = Text.LoadFont("barial.ttf",20);
            Text.SetFontStyle(font, SwinGame.FontStyle.UnderlineFont);

            //SoundEffect effect = Audio.LoadSoundEffect("SwinGameStart.ogg");
            //Music music = Audio.LoadMusic("SwinGameStart.wav");
            //Audio.PlayMusic(music,-1);
            while (Core.WindowCloseRequested() != true)
            {
                Text.DrawText("Hi", System.Drawing.Color.Blue, font, 10, 10);
                
                //Audio.PlaySoundEffect(effect);

                Core.RefreshScreen();
                Core.ProcessEvents();
            }
            //Audio.FreeMusic(ref music);
            //Audio.FreeSoundEffect(ref effect);
            //Graphics.FreeBitmap(ref hey);
            Audio.CloseAudio();
        }
    }
}
