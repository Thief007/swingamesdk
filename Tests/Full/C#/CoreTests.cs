using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using Color = System.Drawing.Color;

namespace Tests
{
    class CoreTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Core Tests");
            result.Add(new MemoryTest());
            result.Add(new TimerTest());
            result.Add(new ScreenTest());
            list.Add(result);
        }

        #endregion

        private class TimerTest : TestSet
        {
            private readonly static string METHS =
                "All of Timer";

            private readonly static string INST =
                "[S]tart and Stop the Timer" + Environment.NewLine +
                "[R]estart Timer";

            public TimerTest() : base(METHS, INST) { }

            private Timer _Timer = Core.CreateTimer();
            private Boolean _IsTimerRunning = false;
            private Boolean _HasTimerStarted = false;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                StartTimer();

                if (Input.WasKeyTyped(Keys.VK_S)) ToggleTimer();
                if (Input.WasKeyTyped(Keys.VK_R)) Core.StartTimer(_Timer);

                Text.DrawText(Convert.ToString(Core.GetTimerTicks(_Timer)), Color.White, GameResources.GameFont("Courier"), 10, 10);

            }

            private void ToggleTimer()
            {
                if (_IsTimerRunning)
                {
                    Core.PauseTimer(_Timer);
                }
                else
                {
                    Core.UnpauseTimer(_Timer);
                }

                _IsTimerRunning = !_IsTimerRunning;
            }

            private void StartTimer()
            {
                if (!_HasTimerStarted)
                {
                    Core.StartTimer(_Timer);
                    _HasTimerStarted = true;
						  _IsTimerRunning = true;
                }
            }
        }

        private class MemoryTest : TestSet
        {
            private readonly static string METHS =
                "Memory Tests";

            private readonly static string INST =
                "[1] Bitmaps" + Environment.NewLine +
                "[2] Sounds" + Environment.NewLine +
                "[3] Fonts" + Environment.NewLine +
                "[4] Sprite" + Environment.NewLine +
                "[5] Matrix" + Environment.NewLine +
                "[6] Timer" + Environment.NewLine +
                "Toggle Full [S]creen";

						private int _Test = 1;
						private int _Count = 0;
						private string _Snd;

            public MemoryTest() : base(METHS, INST) 
						{
							_Snd = Core.GetPathToResource("shock2.wav", ResourceKind.SoundResource);
						}

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.WasKeyTyped(Keys.VK_1)) { _Test = 1; _Count = 0; }
                if (Input.WasKeyTyped(Keys.VK_2)) { _Test = 2; _Count = 0; }

								_Count++;
								
								switch(_Test)
								{
									case 1:
										Text.DrawText("Testing Bitmap Free... " + _Count, Color.White, GameResources.GameFont("Courier"), 10, 10);
										Bitmap b = Graphics.CreateBitmap(100, 100);
										Graphics.FreeBitmap(b);
										break;
									case 2:
										Text.DrawText("Testing Sound Free... " + _Count, Color.White, GameResources.GameFont("Courier"), 10, 10);
										SoundEffect s = Audio.LoadSoundEffect(_Snd);
										Audio.FreeSoundEffect(s);
										break;
								}
            }
        }

        private class ScreenTest : TestSet
        {
            private readonly static string METHS =
                "Screen Tests";

            private readonly static string INST =
                "[3]00 x 200" + Environment.NewLine +
                "[6]40 x 480" + Environment.NewLine +
                "[8]00 x 600" + Environment.NewLine +
                "Toggle Full [S]creen";

            public ScreenTest() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Text.DrawText("Current Screen Resolution: " + Convert.ToString(Core.ScreenWidth()) + "x" + Convert.ToString(Core.ScreenHeight()), Color.White, GameResources.GameFont("Courier"), 10, 10);
    
                if (Input.WasKeyTyped(Keys.VK_3)) Core.ChangeScreenSize(300, 200);
                if (Input.WasKeyTyped(Keys.VK_6)) Core.ChangeScreenSize(640, 480);
                if (Input.WasKeyTyped(Keys.VK_8)) Core.ChangeScreenSize(800, 600);

                if (Input.WasKeyTyped(Keys.VK_S)) Core.ToggleFullScreen();
            }
        }
    }


}
