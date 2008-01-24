using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using Rectangle = System.Drawing.Rectangle;
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
		    if (ClearScreen) Graphics.DrawBitmapOnScreen(GameResources.GameImage("BGA"), (int)(drawIn.X), (int)(drawIn.Y));;	
    		
		    DrawMethodBeingTested();		
		    DrawInstructions();
    		
		    if (Input.WasKeyTyped(Keys.VK_P)) Pass();		    
		    if (Input.WasKeyTyped(Keys.VK_F)) Fail();
		    if (Input.WasKeyTyped(Keys.VK_N)) Skip();
    		
			Graphics.SetClip(drawIn);
			Camera.SetScreenOffset(-drawIn.X, -drawIn.Y);
		    ToRun(drawIn);
			Camera.SetScreenOffset(0, 0);
			Graphics.ResetClip();
        }

        private void DrawInstructions()
        {
            Rectangle rect;
	
            rect = Shapes.CreateRectangle(Consts.INSTRUCTION_LEFT, Consts.INSTRUCTION_TOP, Consts.INSTRUCTION_WIDTH, Consts.INSTRUCTION_HEIGHT);

            Graphics.FillRectangle(Color.Black, rect);
		    Text.DrawTextLines(Instructions, Color.White, Color.Black, GameResources.GameFont("Courier"), FontAlignment.AlignLeft, rect);
        }

        private void DrawMethodBeingTested()
        {
            Graphics.FillRectangleOnScreen(Color.Black, Consts.METHOD_LEFT, Consts.METHOD_TOP, Consts.METHOD_WIDTH, Consts.METHOD_HEIGHT);
            Text.DrawTextOnScreen(MethodBeingTested, Color.White, GameResources.GameFont("Courier"), Consts.METHOD_LEFT, Consts.METHOD_TOP);
        }

        protected abstract void ToRun(Rectangle toDrawIn);
    }
}
