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

        private static Dictionary<string, Bitmap> _Images = new Dictionary<string, Bitmap>();
        private static Dictionary<string, Font> _Fonts = new Dictionary<string, Font>();
        private static Dictionary<string, SoundEffect> _Sounds = new Dictionary<string, SoundEffect>();
        private static Dictionary<string, Music> _Music = new Dictionary<string, Music>();
        private static Dictionary<string, Map> _Maps = new Dictionary<string, Map>();

        private static Bitmap _Background;
        private static Bitmap _Animation;
        private static Bitmap _LoaderFull;
        private static Bitmap _LoaderEmpty;
        private static Font _LoadingFont;
        private static SoundEffect _StartSound;

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
	            _LoadingFont = Text.LoadFont(Core.GetPathToResource("arial.ttf", ResourceKind.FontResource), 12);
	            _StartSound = Audio.LoadSoundEffect(Core.GetPathToResource("SwinGameStart.ogg", ResourceKind.SoundResource));

					_LoaderFull = Graphics.LoadBitmap(Core.GetPathToResource("loader_full.png", ResourceKind.ImageResource));
					_LoaderEmpty = Graphics.LoadBitmap(Core.GetPathToResource("loader_empty.png", ResourceKind.ImageResource));

	            PlaySwinGameIntro();
        }

        public static void PlaySwinGameIntro()
        {
						const int ANI_X = 143, ANI_Y = 134, ANI_W = 546, ANI_H = 327, ANI_V_CELL_COUNT = 6, ANI_CELL_COUNT = 11;

	          Audio.PlaySoundEffect(_StartSound);

	          Core.Sleep(200);

	          for (int i = 0; i < ANI_CELL_COUNT; i++)
	          {
	              Graphics.DrawBitmap(_Background, 0, 0);

	              Graphics.DrawBitmapPart(_Animation, (i / ANI_V_CELL_COUNT) * ANI_W, (i % ANI_V_CELL_COUNT) * ANI_H, ANI_W, ANI_H, ANI_X, ANI_Y);

	              Core.Sleep(20);

	              Core.RefreshScreen();
	              Core.ProcessEvents();
	          }

	          Core.Sleep(1500);
        }

        public static void ShowMessage(String message, int number)
        {   
						const int TX = 310, TY = 493, TW = 200, TH = 25, STEPS = 5, BG_X = 279, BG_Y = 453;

						int fullW = 260 * number / STEPS;
						Graphics.DrawBitmap(_LoaderEmpty, BG_X, BG_Y);
						Graphics.DrawBitmapPart(_LoaderFull, 0, 0, fullW, 66, BG_X, BG_Y);

						Text.DrawTextLines(message, Color.White, Color.Transparent, _LoadingFont, FontAlignment.AlignCenter, TX, TY, TW, TH);
	          Core.RefreshScreen();
	          Core.ProcessEvents();
        }

        public static void EndLoadingScreen(int width, int height)
        {
						Core.ProcessEvents();
						Core.Sleep(500);
	          Graphics.ClearScreen();
	          Core.RefreshScreen();
	          Text.FreeFont(_LoadingFont);
	          Graphics.FreeBitmap(_Background);
	          Graphics.FreeBitmap(_Animation);
		        Graphics.FreeBitmap(_LoaderEmpty);
		        Graphics.FreeBitmap(_LoaderFull);
	          Audio.FreeSoundEffect(_StartSound);
	          Core.ChangeScreenSize(width, height);
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
