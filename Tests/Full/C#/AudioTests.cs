using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;

namespace Tests
{

    class AudioTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Audio Tests");
            result.Add(new PlaySoundEffectTest());
            result.Add(new PlayMusicTest());
            list.Add(result);
        }

        #endregion

        private class PlaySoundEffectTest : TestSet
        {
            private readonly static string METHS =
                "PlaySoundEffect, IsSoundEffectPlaying, StopSoundEffect";

            private readonly static string INST = 
                "pl[a]y sound once" + Environment.NewLine + 
                "play [i]f not playing" + Environment.NewLine + 
                "[s]top sound effect" + Environment.NewLine + 
                "[l]oop sound" + Environment.NewLine +
                "play at 25% [v]olume";

            public PlaySoundEffectTest() : base(METHS, INST){}

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                SoundEffect se = GameResources.GameSound("Shock");
                if(Input.WasKeyTyped(Keys.VK_A)) Audio.PlaySoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_I))
			        if(false == Audio.IsSoundEffectPlaying(se)) 
                        Audio.PlaySoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_S)) Audio.StopSoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_L)) Audio.PlaySoundEffect(se, -1);
                if (Input.WasKeyTyped(Keys.VK_V)) se.Play(0.25f);
            }
        }

        private class PlayMusicTest : TestSet
        {
            private readonly static string METHS =
                "PlayMusic, IsMusicPlaying, StopMusic";

            private readonly static string INST =
                "pl[a]y musix once" + Environment.NewLine +
                "play [i]f not playing" + Environment.NewLine +
                "[s]top music" + Environment.NewLine +
                "[l]oop music" + Environment.NewLine +
                "[up/down] to change volume";

            public PlayMusicTest() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Music se = GameResources.GameMusic("Fast");
                if (Input.WasKeyTyped(Keys.VK_A)) Audio.PlayMusic(se, 1);
                if (Input.WasKeyTyped(Keys.VK_I))
                    if (false == Audio.IsMusicPlaying())
                        Audio.PlayMusic(se);
                if (Input.WasKeyTyped(Keys.VK_S)) Audio.StopMusic();
                if (Input.WasKeyTyped(Keys.VK_L)) Audio.PlayMusic(se);
                if (Input.IsKeyPressed(Keys.VK_UP)) Music.Volume += 0.01f;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) Music.Volume -= 0.01f;


            }
        }
    }
}
