using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace GameProject
{
    public enum ResourceKind
    {     
	    FontResource,
	    ImageResource,
	    SoundResource
    }

    public class Core
    {
        [DllImport("SGSDK.dll")]
        public static extern void OpenGraphicsWindow(String caption, int width, int height);
        [DllImport("SGSDK.dll")]
        public static extern bool WindowCloseRequested();
        [DllImport("SGSDK.dll")]
        public static extern void ProcessEvents();

        [DllImport("SGSDK.dll")]
        public static extern void PlaySoundEffect(IntPtr effect);
        [DllImport("SGSDK.dll")]
        public static extern IntPtr LoadSoundEffect(String path);
        [DllImport("SGSDK.dll")]
        public static extern void OpenAudio();
        [DllImport("SGSDK.dll")]
        public static extern void CloseAudio();
        [DllImport("SGSDK.dll")]
        public static extern void FreeSoundEffect(ref IntPtr effect);
     
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
        
        [DllImport("SGSDK.dll", EntryPoint= "GetColourRGBA")]
        private static extern int DLL_GetColor(Byte red, Byte green, Byte blue, Byte alpha);

        //function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload; cdecl; export;
        //begin
        //result := SGSDK_Core.GetColour(forBitmap, apiColor);
        //end;

        public static Color GetColor(Byte red, Byte green, Byte blue, Byte alpha)
        {
            return Color.FromArgb(alpha, red, green, blue);
        }

        public static Color GetColor(Byte red, Byte green, Byte blue)
        {
            return Color.FromArgb(red, green, blue);
        }
	      
        [DllImport("SGSDK.dll")]
        public static extern int GetFramerate();
	
	    [DllImport("SGSDK.dll")]
        public static extern UInt32 GetTicks();

        [DllImport("SGSDK.dll")]
        public static extern UInt32 Sleep(UInt32 time);
	
        //[DllImport("SGSDK.dll")]
        //public static extern UInt32 getPathToResource(String filename, ResourceKind kind);

	    //function GetPathToResource(filename: String): String; overload; cdecl; export;
	    //begin
		    //result := SGSDK_Core.GetPathToResource(filename);
	    //end;
	            
        [DllImport("SGSDK.dll")]
        public static extern Single Cos(Single angle);

        [DllImport("SGSDK.dll")]
        public static extern Single Sin(Single angle);

	    [DllImport("SGSDK.dll")]
        public static extern Single Tan(Single angle);
        
     }
}
