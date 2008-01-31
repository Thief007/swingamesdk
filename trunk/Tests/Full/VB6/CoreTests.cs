using System;
using System.Collections.Generic;
using System.Text;
using SwinGameVB;
using Color = System.Drawing.Color;

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

            private Timer _Timer =Consts.Core.CreateTimer();
            private Boolean _IsTimerRunning = false;
            private Boolean _HasTimerStarted = false;

            protected override void ToRun(Rectangle toDrawIn)
            {
                StartTimer();

                if (Consts.Input.WasKeyTyped(Keys.VK_S)) ToggleTimer();
                if (Consts.Input.WasKeyTyped(Keys.VK_R))Consts.Core.StartTimer(_Timer);

                Consts.Text.DrawText(Convert.ToString(Consts.Core.GetTimerTicks(_Timer)), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);

            }

            private void ToggleTimer()
            {
                if (_IsTimerRunning)
                {
                   Consts.Core.PauseTimer(_Timer);
                }
                else
                {
                   Consts.Core.UnpauseTimer(_Timer);
                }

                _IsTimerRunning = !_IsTimerRunning;
            }

            private void StartTimer()
            {
                if (!_HasTimerStarted)
                {
                   Consts.Core.StartTimer(_Timer);
                    _HasTimerStarted = true;
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

            protected override void ToRun(Rectangle toDrawIn)
            {
                Consts.Text.DrawText("Current Screen Resolution: " + Convert.ToString(Consts.Core.ScreenWidth()) + "x" + Convert.ToString(Consts.Core.ScreenHeight()), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
    
                if (Consts.Input.WasKeyTyped(Keys.VK_3))Consts.Core.ChangeScreenSize(300, 200);
                if (Consts.Input.WasKeyTyped(Keys.VK_6))Consts.Core.ChangeScreenSize(640, 480);
                if (Consts.Input.WasKeyTyped(Keys.VK_8))Consts.Core.ChangeScreenSize(800, 600);

                if (Consts.Input.WasKeyTyped(Keys.VK_S))Consts.Core.ToggleFullScreen();
            }
        }
    }


}
