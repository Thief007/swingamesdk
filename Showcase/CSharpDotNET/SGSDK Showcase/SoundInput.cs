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
    public static class SoundInput
    {
        private static SoundEffect sound = Audio.LoadSoundEffect(Core.GetPathToResource("Shock.wav", ResourceKind.SoundResource));
        public static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);

        public static void Run()
        {
            for (int i = 0; i < 2000; i++)
            {
                if (Input.IsKeyPressed(SwinGame.Keys.VK_SPACE) && !Audio.IsSoundEffectPlaying(sound))
                {
                    Audio.PlaySoundEffect(sound);
                }

                Text.DrawText("Press Space to play a Sound Effect", Color.White, _Font, 210, 300);

                Overlay.DrawOverlay("Play Sound Effect when hitting a key Example");
                Core.ProcessEvents();
                Core.RefreshScreen();
                Graphics.ClearScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }

            }
        }

    }
}
