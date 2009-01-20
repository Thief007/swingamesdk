using System;
using System.Collections.Generic;
using System.Text;
using SwinGameVB;
using Color = System.Drawing.Color;

namespace Tests
{
    abstract class TestSet
    {
        private string _MethodBeingTested;

        public string MethodBeingTested
        {
            get { return _MethodBeingTested; }
        }
        private string _Instructions;

        public string Instructions
        {
            get { return _Instructions; }
        }
        
        private bool _Passed = false;

        public bool Passed
        {
            get { return _Passed; }
        }
        private bool _Skipped = true;

        public bool Skipped
        {
            get { return _Skipped; }
        }

        private bool _Done = false;

        public bool Done
        {
            get { return _Done; }
        }

        private bool _ClearScreen = true;

        public bool ClearScreen
        {
            get { return _ClearScreen; }
            set { _ClearScreen = value; }
        }

        public TestSet(string method, string instructions)
        {
            _MethodBeingTested = method;
            _Instructions = instructions;
        }

        private void Pass()
        {
            _Passed = true;
            _Done = true;
            _Skipped = false;
        }

        private void Skip()
        {
            _Done = true;
        }

        private void Fail()
        {
            _Passed = false;
            _Done = true;
            _Skipped = false;
        }

        public void Run(Rectangle drawIn)
        {
		    if (ClearScreen) Consts.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BGA"), (int)(drawIn.GetX()), (int)(drawIn.GetY()));;	
    		
		    DrawMethodBeingTested();		
		    DrawInstructions();
    		
		    if (Consts.Input.WasKeyTyped(Keys.VK_P)) Pass();		    
		    if (Consts.Input.WasKeyTyped(Keys.VK_F)) Fail();
		    if (Consts.Input.WasKeyTyped(Keys.VK_N)) Skip();

            Consts.Graphics.SetClip_Rectangle(drawIn);
			Consts.Camera.SetScreenOffset(-drawIn.GetX(), -drawIn.GetY());
		    ToRun(drawIn);
			Consts.Camera.SetScreenOffset(0, 0);
			Consts.Graphics.ResetClip();
        }

        private void DrawInstructions()
        {
            Rectangle rect;
	
            rect =Consts.Shapes.CreateRectangle(Consts.INSTRUCTION_LEFT, Consts.INSTRUCTION_TOP, Consts.INSTRUCTION_WIDTH, Consts.INSTRUCTION_HEIGHT);

            Consts.Graphics.FillRectangle_Rectangle(Color.Black.ToArgb(), rect);
		   Consts.Text.DrawTextLines_Rectangle(Instructions, Color.White.ToArgb(), Color.Black.ToArgb(), GameResources.GameFont("Courier"), FontAlignment.AlignLeft, rect);
        }

        private void DrawMethodBeingTested()
        {
            Consts.Graphics.FillRectangleOnScreen(Color.Black.ToArgb(), Consts.METHOD_LEFT, Consts.METHOD_TOP, Consts.METHOD_WIDTH, Consts.METHOD_HEIGHT);
           Consts.Text.DrawTextOnScreen(MethodBeingTested, Color.White.ToArgb(), GameResources.GameFont("Courier"), Consts.METHOD_LEFT, Consts.METHOD_TOP);
        }

        protected abstract void ToRun(Rectangle toDrawIn);
    }
}
