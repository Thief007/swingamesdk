using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;


namespace SGSDK_Showcase
{
    public static class Intro
    {
        private static Bitmap _Background;
        private static Bitmap _Animation;
        private static Font _LoadingFont;
        private static SoundEffect _StartSound;

        public static void LoadResources()
        {
            int width = Core.ScreenWidth();
            int height = Core.ScreenHeight();

            Core.ChangeScreenSize(800, 600);

            ShowLoadingScreen();

            ShowMessage("Loading fonts...", 0); 
		    //LoadFonts();
		    Core.Sleep(500);
		
		    ShowMessage("Loading images...", 1);
		    //LoadImages();
		    Core.Sleep(500);
		
		    ShowMessage("Loading sounds...", 2);
		    //LoadSounds();
		    Core.Sleep(500);
		
		    ShowMessage("Loading music...", 3);
		    //LoadMusic();
		    Core.Sleep(500);
		
		    //Add game level loading here...
		
		    Core.Sleep(500);
		    ShowMessage("Game loaded...", 4);
		    Core.Sleep(500);
		    EndLoadingScreen(width, height);
        }

        public static void ShowLoadingScreen()
        {
            _Background = Graphics.LoadBitmap(Core.GetPathToResource("SplashBack.png", ResourceKind.ImageResource));
            Graphics.DrawBitmap(_Background, 0, 0);
            Core.RefreshScreen(60);
            Core.ProcessEvents();

		    _Animation = Graphics.LoadBitmap(Core.GetPathToResource("SwinGameAni.png", ResourceKind.ImageResource));
		    _LoadingFont = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
		    _StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource));

            PlaySwinGameIntro();
        }

        public static void PlaySwinGameIntro()
        {
            Core.Sleep(1000);

            Audio.PlaySoundEffect(_StartSound);

            for (int i = 0; i < 14; i++)
            {
                Graphics.DrawBitmap(_Background, 0, 0);

                Graphics.DrawBitmapPart(_Animation, 0, i * 184, 712, 184, 41, 242);

                Core.Sleep(67);

                Core.RefreshScreen(60);
                Core.ProcessEvents();
            }

            Core.Sleep(1000);
        }

        public static void ShowMessage(String message, int number)
        {   
		    Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number));
		    Core.RefreshScreen(60);
		    Core.ProcessEvents();
        }

        public static void EndLoadingScreen(int width, int height)
        {
            Graphics.ClearScreen();
            Core.RefreshScreen(60);
            Text.FreeFont(ref _LoadingFont);
            Graphics.FreeBitmap(ref _Background);
            Graphics.FreeBitmap(ref _Animation);
            Audio.FreeSoundEffect(ref _StartSound);

            Core.ChangeScreenSize(width, height);
        }
 
    }
}
