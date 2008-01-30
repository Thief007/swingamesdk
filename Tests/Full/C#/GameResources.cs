using System;
using System.Collections.Generic;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;


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
            int width = Core.ScreenWidth();
            int height = Core.ScreenHeight();

            Core.ChangeScreenSize(800, 600);

            ShowLoadingScreen();

            ShowMessage("Loading fonts...", 0); 
		    LoadFonts();
		    Core.Sleep(50);
		
		    ShowMessage("Loading images...", 1);
		    LoadImages();
		    Core.Sleep(50);
		
		    ShowMessage("Loading sounds...", 2);
		    LoadSounds();
		    Core.Sleep(50);
		
		    ShowMessage("Loading music...", 3);
		    LoadMusic();
		    Core.Sleep(50);

            ShowMessage("Loading maps...", 4);
		    LoadMaps();
		    Core.Sleep(50);
		
		    //Add game level loading here...
		
		    Core.Sleep(50);
		    ShowMessage("Game loaded...", 5);
		    Core.Sleep(50);
		    EndLoadingScreen(width, height);
        }

        public static void ShowLoadingScreen()
        {
            _Background = Graphics.LoadBitmap(Core.GetPathToResource("SplashBack.png", ResourceKind.ImageResource));
            Graphics.DrawBitmap(_Background, 0, 0);
            Core.RefreshScreen();
            Core.ProcessEvents();

		    _Animation = Graphics.LoadBitmap(Core.GetPathToResource("SwinGameAni.png", ResourceKind.ImageResource));
		    _LoadingFont = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
		    _StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource));

            PlaySwinGameIntro();
        }

        public static void PlaySwinGameIntro()
        {
            Core.Sleep(300);

            Audio.PlaySoundEffect(_StartSound);

            for (int i = 0; i < 14; i++)
            {
                Graphics.DrawBitmap(_Background, 0, 0);
                Graphics.DrawBitmapPart(_Animation, (i / 7) * 712, (i % 7) * 184, 712, 184, 41, 242);

                Core.Sleep(67);

                Core.RefreshScreen();
                Core.ProcessEvents();
            }

            Core.Sleep(1500);
        }

        public static void ShowMessage(String message, int number)
        {   
		    Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number));
		    Core.RefreshScreen();
		    Core.ProcessEvents();
        }

        public static void EndLoadingScreen(int width, int height)
        {
				Core.ProcessEvents();
				Core.Sleep(500);
				Core.ChangeScreenSize(width, height);
            Graphics.ClearScreen();
            Core.RefreshScreen();
				Core.ProcessEvents();
			
				Graphics.FreeBitmap(_Animation);
            Text.FreeFont(_LoadingFont);
            Graphics.FreeBitmap(_Background);
            Audio.FreeSoundEffect(_StartSound);
        }

        private static void NewMap(String mapName)
        {
				_Maps.Add(mapName, MappyLoader.LoadMap(mapName));
        }

        private static void NewFont(String fontName, String filename, int size)
        {
				_Fonts.Add(fontName, Text.LoadFont(Core.GetPathToResource(filename, ResourceKind.FontResource), size));
        }

        private static void NewImage(String imageName, String filename)
        {
				_Images.Add(imageName, Graphics.LoadBitmap(Core.GetPathToResource(filename, ResourceKind.ImageResource)));
        }

		  private static void NewTransparentColorImage(String imageName, String fileName, Color transColor)
        {
            _Images.Add(imageName, Graphics.LoadBitmap(Core.GetPathToResource(fileName, ResourceKind.ImageResource), true, transColor));
        }

        private static void NewTransparentColourImage(String imageName, String fileName, Color transColor)
        {
            NewTransparentColorImage(imageName, fileName, transColor);
        }

        private static void NewSound(String soundName, String filename)
        {
				_Sounds.Add(soundName, Audio.LoadSoundEffect(Core.GetPathToResource(filename, ResourceKind.SoundResource)));
        }

        private static void NewMusic(String musicName, String filename)
        {
				_Music.Add(musicName, Audio.LoadMusic(Core.GetPathToResource(filename, ResourceKind.SoundResource)));
        }

        private static void FreeFonts()
        {
            foreach(Font f in _Fonts.Values)
            {
                Text.FreeFont(f);
            }
				_Fonts.Clear();
        }

        private static void FreeImages()
        {
            foreach(Bitmap b in _Images.Values)
            {
                Graphics.FreeBitmap(b);
            }
				_Images.Clear();
        }

        private static void FreeSounds()
        {
            foreach(SoundEffect ef in _Sounds.Values)
            {
                Audio.FreeSoundEffect(ef);
            }
				_Sounds.Clear();
        }

        private static void FreeMusic()
        {
            foreach (Music m in _Music.Values)
            {
                Audio.FreeMusic(m);
            }
				_Music.Clear();
        }

        private static void FreeMaps()
        {
            foreach (Map m in _Maps.Values)
            {
                MappyLoader.FreeMap(m);
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
