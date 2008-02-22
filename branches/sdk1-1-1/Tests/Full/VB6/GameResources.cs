using System;
using System.Collections.Generic;
using System.Drawing;

using SwinGameVB;
using Graphics = SwinGameVB.Graphics;
using Bitmap = SwinGameVB.Bitmap;
using Font = SwinGameVB.Fonts;
using FontStyle = SwinGameVB.FontStyle;


namespace Tests
{
    public static class GameResources
    {
		  private static Dictionary<string, Bitmap> _Images = new Dictionary<string, Bitmap>();
		  private static Dictionary<string, Font> _Fonts = new Dictionary<string, Font>();
		  private static Dictionary<string, SoundEffect> _Sounds = new Dictionary<string, SoundEffect>();
		  private static Dictionary<string, Music> _Music = new Dictionary<string, Music>();
		  private static Dictionary<string, Map> _Maps = new Dictionary<string, Map>();

        private static Bitmap _Background;
        private static Bitmap _Animation;
        private static Font _LoadingFont;
        private static SoundEffect _StartSound;


        private static void LoadFonts()
        {
            NewFont("ArialLarge", "arial.ttf", 80);
            NewFont("Courier", "cour.ttf", 15);
            NewFont("CourierLarge", "cour.ttf", 28);
        }

        private static void LoadImages()
        {
            NewImage("BallImage1", "ball.png");
		    NewImage("BallImage2", "ball2.png");
		    NewImage("SmallBall", "ball_small.png");
		    NewImage("Running", "running.png");
		    NewImage("Explosion", "Explosion.png");
		    NewImage("Ship", "ship.png");
		    NewImage("Sea", "sea.png");
			NewImage("BGA", "BackgroundDrawArea.png");
			NewImage("BG", "BackgroundMain.png");
            NewImage("ExplosionBlue", "explosion_blue.jpg");

            NewImage("Frame1", "F01.png");
		    NewImage("Frame2", "F02.png");
		    NewImage("Frame3", "F03.png");
		    NewImage("Frame4", "F04.png");
		    NewImage("Frame5", "F05.png");
		    NewImage("Frame6", "F06.png");
		    NewImage("enShip", "enShip.png");

		    NewTransparentColourImage("BlueExplosion", "explosion_pro.jpg", Color.Black);
            NewTransparentColourImage("BlueExplosion2", "explosion_blue.jpg", Color.Black);
		    for (int i = 0; i < 40; i++)
		    {
                NewTransparentColourImage("Explode_" + Convert.ToString(i), "explode_" + Convert.ToString(i) + ".jpg", Color.Black);
		    }
        }

        private static void LoadSounds()
        {
            NewSound("Shock", "shock.wav");   
        }

        private static void LoadMusic()
        {
            NewMusic("Fast", "Fast.mp3");   
        }

        private static void LoadMaps()
        {
            NewMap("test");
            NewMap("test3");
        }

        public static void LoadResources()
        {
            int width =Consts.Core.ScreenWidth();
            int height =Consts.Core.ScreenHeight();

           Consts.Core.ChangeScreenSize(800, 600);

            ShowLoadingScreen();

            ShowMessage("Loading fonts...", 0); 
		    LoadFonts();
		   Consts.Core.Sleep(50);
		
		    ShowMessage("Loading images...", 1);
		    LoadImages();
		   Consts.Core.Sleep(50);
		
		    ShowMessage("Loading sounds...", 2);
		    LoadSounds();
		   Consts.Core.Sleep(50);
		
		    ShowMessage("Loading music...", 3);
		    LoadMusic();
		   Consts.Core.Sleep(50);

            ShowMessage("Loading maps...", 4);
		    LoadMaps();
		   Consts.Core.Sleep(50);
		
		    //Add game level loading here...
		
		   Consts.Core.Sleep(50);
		    ShowMessage("Game loaded...", 5);
		   Consts.Core.Sleep(50);
		    EndLoadingScreen(width, height);
        }

        public static void ShowLoadingScreen()
        {
            _Background = Consts.Graphics.LoadBitmap(Consts.Core.GetPathToResource("SplashBack.png", ResourceKind.ImageResource));
            Consts.Graphics.DrawBitmap(_Background, 0, 0);
           Consts.Core.RefreshScreen();
           Consts.Core.ProcessEvents();

           _Animation = Consts.Graphics.LoadBitmap(Consts.Core.GetPathToResource("SwinGameAni.png", ResourceKind.ImageResource));
           _LoadingFont = Consts.Text.LoadFont(Consts.Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
           _StartSound = Consts.Audio.LoadSoundEffect(Consts.Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource));

            PlaySwinGameIntro();
        }

        public static void PlaySwinGameIntro()
        {
           Consts.Core.Sleep(300);

           Consts.Audio.PlaySoundEffect(_StartSound);

            for (int i = 0; i < 14; i++)
            {
                Consts.Graphics.DrawBitmap(_Background, 0, 0);
                Consts.Graphics.DrawBitmapPart(_Animation, (i / 7) * 712, (i % 7) * 184, 712, 184, 41, 242);

               Consts.Core.Sleep(67);

               Consts.Core.RefreshScreen();
               Consts.Core.ProcessEvents();
            }

           Consts.Core.Sleep(1500);
        }

        public static void ShowMessage(String message, int number)
        {   
		   Consts.Text.DrawText(message, Color.Red.ToArgb(), _LoadingFont, 240, 20 + (25 * number));
		   Consts.Core.RefreshScreen();
		   Consts.Core.ProcessEvents();
        }

        public static void EndLoadingScreen(int width, int height)
        {
            Consts.Core.ProcessEvents();
            Consts.Core.Sleep(500);
            Consts.Core.ChangeScreenSize(width, height);
            Consts.Graphics.ClearScreen();
           Consts.Core.RefreshScreen();
           Consts.Core.ProcessEvents();

           Consts.Graphics.FreeBitmap(_Animation);
           Consts.Text.FreeFont(_LoadingFont);
            Consts.Graphics.FreeBitmap(_Background);
           Consts.Audio.FreeSoundEffect(_StartSound);
        }

        private static void NewMap(String mapName)
        {
				_Maps.Add(mapName,Consts.MappyLoader.LoadMap(mapName));
        }

        private static void NewFont(String fontName, String filename, int size)
        {
            _Fonts.Add(fontName, Consts.Text.LoadFont(Consts.Core.GetPathToResource(filename, ResourceKind.FontResource), size));
        }

        private static void NewImage(String imageName, String filename)
        {
            _Images.Add(imageName, Consts.Graphics.LoadBitmap(Consts.Core.GetPathToResource(filename, ResourceKind.ImageResource)));
        }

		  private static void NewTransparentColorImage(String imageName, String fileName, Color transColor)
        {
            _Images.Add(imageName, Consts.Graphics.LoadBitmap_Transparent(Consts.Core.GetPathToResource(fileName, ResourceKind.ImageResource), true, transColor.ToArgb()));
        }

        private static void NewTransparentColourImage(String imageName, String fileName, Color transColor)
        {
            NewTransparentColorImage(imageName, fileName, transColor);
        }

        private static void NewSound(String soundName, String filename)
        {
            _Sounds.Add(soundName, Consts.Audio.LoadSoundEffect(Consts.Core.GetPathToResource(filename, ResourceKind.SoundResource)));
        }

        private static void NewMusic(String musicName, String filename)
        {
            _Music.Add(musicName, Consts.Audio.LoadMusic(Consts.Core.GetPathToResource(filename, ResourceKind.SoundResource)));
        }

        private static void FreeFonts()
        {
            foreach(Font f in _Fonts.Values)
            {
               Consts.Text.FreeFont(f);
            }
				_Fonts.Clear();
        }

        private static void FreeImages()
        {
            foreach(Bitmap b in _Images.Values)
            {
                Consts.Graphics.FreeBitmap(b);
            }
				_Images.Clear();
        }

        private static void FreeSounds()
        {
            foreach(SoundEffect ef in _Sounds.Values)
            {
               Consts.Audio.FreeSoundEffect(ef);
            }
				_Sounds.Clear();
        }

        private static void FreeMusic()
        {
            foreach (Music m in _Music.Values)
            {
               Consts.Audio.FreeMusic(m);
            }
				_Music.Clear();
        }

        private static void FreeMaps()
        {
            foreach (Map m in _Maps.Values)
            {
               Consts.MappyLoader.FreeMap(m);
            }
				_Maps.Clear();
        }

        /// <summary>
        /// Frees All Resources
        /// </summary>
        public static void FreeResources()
        {
            FreeFonts();
            FreeImages();
            FreeMusic();
            FreeSounds();
            FreeMaps();
        }

        public static Font GameFont(String font)
        {
            return _Fonts[font];
        }

        public static Bitmap GameImage(String image)
        {
				return _Images[image];
        }

        public static SoundEffect GameSound(String sound)
        {
            return _Sounds[sound];
        }

        public static Music GameMusic(String music)
        {
            return _Music[music];
        }

        public static Map GameMap(String map)
        {
            return _Maps[map];
        }
    }
}
