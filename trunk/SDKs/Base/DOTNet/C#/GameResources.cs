using System;
using System.Drawing;
using SwinGame;
using System.Collections.Generic;

using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;


namespace GameResources
{
    /// <summary>
    /// The Resources Class stores all of the Games Media Resources, such as Images, Fonts
    /// Sounds, Music, and Maps.
    /// </summary>
    public static class Resources
    {
        private static void LoadFonts()
        {
            NewFont("ArialLarge", "arial.ttf", 80);
            NewFont("Courier", "cour.ttf", 16);
        }

        private static void LoadImages()
        {
        }

        private static void LoadSounds()
        {
        }

        private static void LoadMusic()
        {
        }

        private static void LoadMaps()
        {
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

        /// <summary>
        /// Loads Resources
        /// </summary>
        public static void LoadResources()
        {
            int width = Core.ScreenWidth();
            int height = Core.ScreenHeight();

            Core.ChangeScreenSize(800, 600);

            ShowLoadingScreen();

            ShowMessage("Loading fonts...", 0);
            LoadFonts();
            Core.Sleep(100);

            ShowMessage("Loading images...", 1);
            LoadImages();
            Core.Sleep(100);

            ShowMessage("Loading sounds...", 2);
            LoadSounds();
            Core.Sleep(100);

            ShowMessage("Loading music...", 3);
            LoadMusic();
            Core.Sleep(100);

            ShowMessage("Loading maps...", 4);
            LoadMaps();
            Core.Sleep(100);

            //Add game level loading here...

            Core.Sleep(100);
            ShowMessage("Game loaded...", 5);
            Core.Sleep(100);
            EndLoadingScreen(width, height);
        }

        private static void ShowLoadingScreen()
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

        private static void PlaySwinGameIntro()
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

        private static void ShowMessage(String message, int number)
        {
						const int TX = 310, TY = 493, TW = 200, TH = 25, STEPS = 5, BG_X = 279, BG_Y = 453;

						int fullW = 260 * number / STEPS;
						Graphics.DrawBitmap(_LoaderEmpty, BG_X, BG_Y);
						Graphics.DrawBitmapPart(_LoaderFull, 0, 0, fullW, 66, BG_X, BG_Y);

						Text.DrawTextLines(message, Color.White, Color.Transparent, _LoadingFont, FontAlignment.AlignCenter, TX, TY, TW, TH);
            Core.RefreshScreen();
            Core.ProcessEvents();
        }

        private static void EndLoadingScreen(int width, int height)
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
                //_ImagesStr[i] = String.Empty;
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
            Core.ProcessEvents();
        }

        /// <summary>
        /// Gets the Desired Font from the Loaded Resources
        /// </summary>
        /// <param name="font">Font to Get</param>
        /// <returns>The Font</returns>
        public static Font GameFont(String font)
        {
            return _Fonts[font];
        }

        /// <summary>
        /// Gets the Desired Image from the Loaded Resources
        /// </summary>
        /// <param name="image">Image to Get</param>
        /// <returns>The Image</returns>
        public static Bitmap GameImage(String image)
        {
				return _Images[image];
        }

        /// <summary>
        /// Gets the Desired Sound from the Loaded Resources
        /// </summary>
        /// <param name="sound">Sound to Get</param>
        /// <returns>The Sound</returns>
        public static SoundEffect GameSound(String sound)
        {
            return _Sounds[sound];
        }

        /// <summary>
        /// Gets the Desired Music from the Loaded Resources
        /// </summary>
        /// <param name="music">Music to get</param>
        /// <returns>The Music</returns>
        public static Music GameMusic(String music)
        {
            return _Music[music];
        }

        /// <summary>
        /// Gets the Desired Map from the Loaded Resources
        /// </summary>
        /// <param name="map">Map to get</param>
        /// <returns>The map</returns>
        public static Map GameMap(String map)
        {
            return _Maps[map];
        }
    }
}