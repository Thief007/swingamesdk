using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using SwinGame;
using Bitmap = SwinGame.Bitmap;
using Graphics = SwinGame.Graphics;
using SoundEffect = SwinGame.SoundEffect;

namespace GameProject
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("here");
            Core.OpenGraphicsWindow("Hi", 800, 600);

            Bitmap hey = Graphics.LoadBitmap("SwinGameAni.png");
            
            Audio.OpenAudio();

            Input.

            //SoundEffect effect = Audio.LoadSoundEffect("SwinGameStart.ogg");
            //Music music = Audio.LoadMusic("SwinGameStart.wav");
            //Audio.PlayMusic(music,2);
            while (Core.WindowCloseRequested() != true)
            {

            
                //Audio.PlaySoundEffect(effect);

                Core.RefreshScreen();
                Core.ProcessEvents();
            }
            //Audio.FreeMusic(ref music);
            //Audio.FreeSoundEffect(ref effect);
            Audio.CloseAudio();
        }
    }
}
