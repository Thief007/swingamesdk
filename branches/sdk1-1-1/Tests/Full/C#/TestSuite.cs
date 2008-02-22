using System;
using System.Collections.Generic;
using System.Text;
using Rectangle = System.Drawing.Rectangle;
using Color = System.Drawing.Color;
using SwinGame;

namespace Tests
{
    class TestSuite
    {
        public TestSuite(string title)
        {
            _Title = title;
        }

        private string _Title;

        public string Title
        {
            get { return _Title; }
        }

        private List<TestSet> _Tests = new List<TestSet>();

        public void Run()
        {
            Rectangle testDrawIn;
            bool skip = false;

            testDrawIn = Shapes.CreateRectangle(Consts.TEST_IN_LEFT, Consts.TEST_IN_TOP, Consts.TEST_IN_WIDTH, Consts.TEST_IN_HEIGHT);

            Graphics.DrawBitmapOnScreen(GameResources.GameImage("BG"), 0, 0);
            DrawTitle(Title);
            DrawGeneralInstructions();

            foreach (TestSet t in _Tests)
            {
                do
                {
                    if (t.ClearScreen)
                    {
                        
                        Graphics.DrawBitmapOnScreen(GameResources.GameImage("BG"), 0, 0);
                        DrawTitle(Title);
                        DrawGeneralInstructions();
                    }

                    Core.ProcessEvents();

                    t.Run(testDrawIn);

                    if (Input.WasKeyTyped(Keys.VK_ESCAPE)) skip = true;

                    Core.RefreshScreen();
                } while (false == (Core.WindowCloseRequested() || t.Done || skip));

                if (Core.WindowCloseRequested() || skip) break;
            }
        }
        private void DrawGeneralInstructions()
        {
            const string INST = "Press: [p]ass, [f]ail, [esc] skip suite, [n]ext test";
			Color fg = Color.FromArgb(255, 33, 118, 182);
            Text.DrawTextOnScreen(INST, fg, GameResources.GameFont("Courier"), Consts.GENERAL_INST_LEFT, Consts.GENERAL_INST_TOP);
        }

        private void DrawTitle(string title)
        {
		    //Graphics.FillRectangleOnScreen(Color.Black, 0, 0, Consts.SCREEN_WIDTH, Consts.TITLE_HEIGHT);
	        Text.DrawTextOnScreen(title, Color.White, GameResources.GameFont("CourierLarge"), Consts.TITLE_TEXT_LEFT, Consts.TITLE_TEXT_TOP);            
        }

        public void Add(TestSet s)
        {
            _Tests.Add(s);
        }

        public void SaveResults(System.IO.TextWriter writer)
        {
		    writer.WriteLine("Suite: {0}", Title);

		    foreach(TestSet t in _Tests)
            {
			    if (t.Skipped)      writer.Write(" --  skip  --");
				else if (t.Passed)  writer.Write("    passed   ");
				else 			    writer.Write(" ** FAILED **");
				writer.WriteLine(" {0}", t.MethodBeingTested);
		    }
        }
    }
}
