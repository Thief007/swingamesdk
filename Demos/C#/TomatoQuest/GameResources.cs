using System;
using System.Collections.Generic;

using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;


namespace GameResources
{
    public static class Resources
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
            NewFont("Courier", "cour.ttf", 16);
            NewFont("Arial", "arial.ttf", 20);
            NewFont("SmallCourier", "cour.ttf", 11);

            Text.SetFontStyle(Resources.GameFont("SmallCourier"), FontStyle.BoldFont);
        }

        private static void LoadImages()
        {
            NewImage("Hero", "Hero.png");
            NewImage("Overlay", "Overlay.png");
            NewImage("Health", "Health.png");
            NewImage("Mana", "Mana.png");
            NewImage("HealthVial", "HealthVial.png");
            NewImage("Slash Up", "Slash_up.png");
            NewImage("Slash Down", "Slash_down.png");
            NewImage("Slash Left", "Slash_left.png");
            NewImage("Slash Right", "Slash_right.png");
            NewImage("Healer", "Healer.png");
            NewImage("Critter", "Critter.png");
            NewImage("Thief", "Thief.png");
            NewImage("ThiefLeader", "ThiefLeader.png");
            NewImage("Tomato", "Tomato.png");
            NewImage("Logo", "Logo.png");
        }

        private static void LoadSounds()
        {
            NewSound("Swing", "Swing.wav");
            NewSound("Heal", "Heal.wav");
            NewSound("Critical", "Critical.wav");
            NewSound("Hit", "Hit.wav");
        }

        private static void LoadMusic()
        {
				NewMusic("Theme", "TomatoQuest.mp3");
        }

        private static void LoadMaps()
        {
            NewMap("Level1");
        }

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
            Core.Sleep(400);

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
            Graphics.ClearScreen();
            Core.RefreshScreen();
            Text.FreeFont(_LoadingFont);
            Graphics.FreeBitmap(_Background);
            Graphics.FreeBitmap(_Animation);
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