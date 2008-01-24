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

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                SoundEffect se = GameResources.GameSound("Shock");
                if(Input.WasKeyTyped(Keys.VK_A)) Audio.PlaySoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_I))
			        if(false == Audio.IsSoundEffectPlaying(se)) 
                        Audio.PlaySoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_S)) Audio.StopSoundEffect(se);
		        if(Input.WasKeyTyped(Keys.VK_L)) Audio.PlaySoundEffect(se, -1);
            }
        }
    }
}
