using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace GameProject
{
    public enum ResourceKind
    {     
	    FontResource,
	    ImageResource,
	    SoundResource
    }

    public class SwinGame
    {
        [DllImport("SGSDK.dll")]
        public static extern void OpenGraphicsWindow(String caption, int width, int height);
        [DllImport("SGSDK.dll")]
        public static extern bool WindowCloseRequested();
        [DllImport("SGSDK.dll")]
        public static extern void ProcessEvents();

        [DllImport("SGSDK.dll")]
        public static extern void SetIcon(String iconFilename);

        [DllImport("SGSDK.dll")]
        public static extern void ChangeScreenSize(int width, int height);

	    [DllImport("SGSDK.dll")]
        public static extern void ToggleFullScreen();

        [DllImport("SGSDK.dll")]
        public static extern void RefreshScreen();
	
        [DllImport("SGSDK.dll")]
        public static extern void TakeScreenshot(String basename);
	
        [DllImport("SGSDK.dll")]
        public static extern int ScreenWidth();
	
        [DllImport("SGSDK.dll")]
        public static extern int ScreenHeight();

	    //function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload; cdecl; export;
	    //begin
		    //result := SGSDK_Core.GetColour(forBitmap, apiColor);
	    //end;

        //[DllImport("SGSDK.dll")]
        //public static extern Colour GetColour(Byte red, Byte green, Byte blue, Byte alpha);
        
        [DllImport("SGSDK.dll")]
        public static extern UInt32 GetColour(Byte red, Byte green, Byte blue, Byte alpha);
	
	    //function	GetColour(red, green, blue : Byte) : Colour; overload; cdecl; export;
	    //begin
		    //result := SGSDK_Core.GetColour(red, green, blue);	
	    //end;

        [DllImport("SGSDK.dll")]
        public static extern int GetFramerate();
	
	    [DllImport("SGSDK.dll")]
        public static extern UInt32 GetTicks();

        [DllImport("SGSDK.dll")]
        public static extern UInt32 Sleep(UInt32 time);
	
        [DllImport("SGSDK.dll")]
        public static extern UInt32 getPathToResource(String filename, ResourceKind kind);

	    //function GetPathToResource(filename: String): String; overload; cdecl; export;
	    //begin
		    //result := SGSDK_Core.GetPathToResource(filename);
	    //end;
	            
	    //procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr); cdecl; export;
	    //begin
		    //SGSDK_Core.RegisterEventProcessor(handle, handle2);
	    //end;
	
        [DllImport("SGSDK.dll")]
        public static extern Single Cos(Single angle);

        [DllImport("SGSDK.dll")]
        public static extern Single Sin(Single angle);

	    [DllImport("SGSDK.dll")]
        public static extern Single Tan(Single angle);
    }
}
