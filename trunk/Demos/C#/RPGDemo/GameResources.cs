using System;
using System.Collections.Generic;
using System.Windows.Forms;
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
        private static Bitmap[] _Images = new Bitmap[0];
        private static Font[] _Fonts = new Font[0];
        private static SoundEffect[] _Sounds = new SoundEffect[0];
        private static Music[] _Music = new Music[0];
        private static Map[] _Maps = new Map[0];

        private static String[] _ImagesStr = new String[0];
        private static String[] _FontsStr = new String[0];
        private static String[] _SoundsStr = new String[0];
        private static String[] _MusicStr = new String[0];
        private static String[] _MapsStr = new String[0];

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

                Graphics.DrawBitmapPart(_Animation, 0, i * 184, 712, 184, 41, 242);

                Core.Sleep(67);

                Core.RefreshScreen();
                Core.ProcessEvents();
            }

            Core.Sleep(400);
        }

        public static void ShowMessage(String message, int number)
        {
            Text.DrawText(message, Color.Red, _LoadingFont, 240, 20 + (25 * number));
            Core.RefreshScreen();
            Core.ProcessEvents();
        }

        public static void EndLoadingScreen(int width, int height)
        {
            Graphics.ClearScreen();
            Core.RefreshScreen();
            Text.FreeFont(ref _LoadingFont);
            Graphics.FreeBitmap(ref _Background);
            Graphics.FreeBitmap(ref _Animation);
            Audio.FreeSoundEffect(ref _StartSound);

            Core.ChangeScreenSize(width, height);
        }

        private static void NewMap(String mapName)
        {
            Array.Resize(ref _Maps, _Maps.Length + 1);
            Array.Resize(ref _MapsStr, _MapsStr.Length + 1);
            _Maps[_Maps.Length - 1] = MappyLoader.LoadMap(mapName);
            _MapsStr[_Maps.Length - 1] = mapName;
        }

        private static void NewFont(String fontName, String filename, int size)
        {
            Array.Resize(ref _Fonts, _Fonts.Length + 1);
            Array.Resize(ref _FontsStr, _FontsStr.Length + 1);
            _Fonts[_Fonts.Length - 1] = Text.LoadFont(Core.GetPathToResource(filename, ResourceKind.FontResource), size);
            _FontsStr[_FontsStr.Length - 1] = fontName;
        }

        private static void NewImage(String imageName, String filename)
        {
            Array.Resize(ref _Images, _Images.Length + 1);
            Array.Resize(ref _ImagesStr, _ImagesStr.Length + 1);
            _Images[_Images.Length - 1] = Graphics.LoadBitmap(Core.GetPathToResource(filename, ResourceKind.ImageResource));
            _ImagesStr[_ImagesStr.Length - 1] = imageName;
        }

        private static void NewSound(String soundName, String filename)
        {
            Array.Resize(ref _Sounds, _Sounds.Length + 1);
            Array.Resize(ref _SoundsStr, _SoundsStr.Length + 1);
            _Sounds[_Sounds.Length - 1] = Audio.LoadSoundEffect(Core.GetPathToResource(filename, ResourceKind.SoundResource));
            _SoundsStr[_SoundsStr.Length - 1] = soundName;
        }

        private static void NewMusic(String musicName, String filename)
        {
            Array.Resize(ref _Music, _Music.Length + 1);
            Array.Resize(ref _MusicStr, _MusicStr.Length + 1);
            _Music[_Music.Length - 1] = Audio.LoadMusic(Core.GetPathToResource(filename, ResourceKind.SoundResource));
            _MusicStr[_Music.Length - 1] = musicName;
        }

        private static void FreeFonts()
        {
            for (int i = 0; i < _Fonts.Length - 1; i++)
            {
                Text.FreeFont(ref _Fonts[i]);
                _FontsStr[i] = String.Empty;
            }
        }

        private static void FreeImages()
        {
            for (int i = 0; i < _Images.Length - 1; i++)
            {
                Graphics.FreeBitmap(ref _Images[i]);
                _ImagesStr[i] = String.Empty;
            }
        }

        private static void FreeSounds()
        {
            for (int i = 0; i < _Sounds.Length - 1; i++)
            {
                Audio.FreeSoundEffect(ref  _Sounds[i]);
                _SoundsStr[i] = String.Empty;
            }
        }

        private static void FreeMusic()
        {
            for (int i = 0; i < _Music.Length - 1; i++)
            {
                Audio.FreeMusic(ref _Music[i]);
                _MusicStr[i] = String.Empty;
            }
        }

        private static void FreeMaps()
        {
            for (int i = 0; i < _Maps.Length - 1; i++)
            {
                MappyLoader.FreeMap(_Maps[i]);
                _MapsStr[i] = String.Empty;
            }
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
            for (int i = 0; i < _FontsStr.Length; i++)
            {
                if (_FontsStr[i].Equals(font))
                {
                    return _Fonts[i];
                }
            }
            throw new SwinGameException("Could not find Font " + font);
        }

        public static Bitmap GameImage(String image)
        {
            for (int i = 0; i < _ImagesStr.Length; i++)
            {
                if (_ImagesStr[i].Equals(image))
                {
                    return _Images[i];
                }
            }
            throw new SwinGameException("Could not find Image " + image);
        }

        public static SoundEffect GameSound(String sound)
        {
            for (int i = 0; i < _SoundsStr.Length; i++)
            {
                if (_SoundsStr[i].Equals(sound))
                {
                    return _Sounds[i];
                }
            }
            throw new SwinGameException("Could not find Sound " + sound);
        }

        public static Music GameMusic(String music)
        {
            for (int i = 0; i < _MusicStr.Length; i++)
            {
                if (_MusicStr[i].Equals(music))
                {
                    return _Music[i];
                }
            }
            throw new SwinGameException("Could not find Music " + music);
        }

        public static Map GameMap(String map)
        {
            for (int i = 0; i < _MapsStr.Length; i++)
            {
                if (_MapsStr[i].Equals(map))
                {
                    return _Maps[i];
                }
            }
            throw new SwinGameException("Could not find Map " + map);
        }

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
            NewMap("Level1");
        }

    }
}