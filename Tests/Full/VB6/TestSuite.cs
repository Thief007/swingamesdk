using System;
using System.Collections.Generic;
using System.Text;
using Rectangle = System.Drawing.Rectangle;
using Color = System.Drawing.Color;
using SwinGameVB;

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
            SwinGameVB.Rectangle testDrawIn;
            bool skip = false;

            testDrawIn =Consts.Shapes.CreateRectangle(Consts.TEST_IN_LEFT, Consts.TEST_IN_TOP, Consts.TEST_IN_WIDTH, Consts.TEST_IN_HEIGHT);

            Consts.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BG"), 0, 0);
            DrawTitle(Title);
            DrawGeneralInstructions();

            foreach (TestSet t in _Tests)
            {
                do
                {
                    if (t.ClearScreen)
                    {
                        
                        Consts.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BG"), 0, 0);
                        DrawTitle(Title);
                        DrawGeneralInstructions();
                    }

                   Consts.Core.ProcessEvents();

                    t.Run(testDrawIn);

                    if (Consts.Input.WasKeyTyped(Keys.VK_ESCAPE)) skip = true;

                   Consts.Core.RefreshScreen_WithFrame(65);
               } while (false == (Consts.Core.WindowCloseRequested() || t.Done || skip));

               if (Consts.Core.WindowCloseRequested() || skip) break;
            }
        }
        private void DrawGeneralInstructions()
        {
            const string INST = "Press: [p]ass, [f]ail, [esc] skip suite, [n]ext test";
			Color fg = Color.FromArgb(255, 33, 118, 182);
           Consts.Text.DrawTextOnScreen(INST, fg.ToArgb(), GameResources.GameFont("Courier"), Consts.GENERAL_INST_LEFT, Consts.GENERAL_INST_TOP);
        }

        private void DrawTitle(string title)
        {
		    //Consts.Graphics.FillRectangleOnScreen(Color.Black.ToArgb(), 0, 0, Consts.SCREEN_WIDTH, Consts.TITLE_HEIGHT);
	       Consts.Text.DrawTextOnScreen(title, Color.White.ToArgb(), GameResources.GameFont("CourierLarge"), Consts.TITLE_TEXT_LEFT, Consts.TITLE_TEXT_TOP);            
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
