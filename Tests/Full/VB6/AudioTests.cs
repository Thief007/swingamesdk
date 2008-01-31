using System;
using System.Collections.Generic;
using System.Text;
using SwinGameVB;

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
                "[l]oop sound";

            public PlaySoundEffectTest() : base(METHS, INST){}

            protected override void ToRun(Rectangle toDrawIn)
            {
                SoundEffect se = GameResources.GameSound("Shock");
                if(Consts.Input.WasKeyTyped(Keys.VK_A))Consts.Audio.PlaySoundEffect(se);
		        if(Consts.Input.WasKeyTyped(Keys.VK_I))
			        if(false ==Consts.Audio.IsSoundEffectPlaying(se)) 
                       Consts.Audio.PlaySoundEffect(se);
		        if(Consts.Input.WasKeyTyped(Keys.VK_S))Consts.Audio.StopSoundEffect(se);
		        if(Consts.Input.WasKeyTyped(Keys.VK_L))Consts.Audio.PlaySoundEffect_Loop(se, -1);
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
                "[l]oop music";

            public PlayMusicTest() : base(METHS, INST) { }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Music se = GameResources.GameMusic("Fast");
                if (Consts.Input.WasKeyTyped(Keys.VK_A))Consts.Audio.PlayMusic_Loop(se, 1);
                if (Consts.Input.WasKeyTyped(Keys.VK_I))
                    if (false ==Consts.Audio.IsMusicPlaying(se))
                       Consts.Audio.PlayMusic(se);
                if (Consts.Input.WasKeyTyped(Keys.VK_S))Consts.Audio.StopMusic();
                if (Consts.Input.WasKeyTyped(Keys.VK_L))Consts.Audio.PlayMusic(se);
            }
        }
    }
}
