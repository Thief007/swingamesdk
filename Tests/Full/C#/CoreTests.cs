using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Drawing;

namespace Tests
{
    class CoreTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Core Tests");
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
