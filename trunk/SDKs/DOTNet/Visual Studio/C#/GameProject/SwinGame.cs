using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    public enum ResourceKind
    {     
	    FontResource,
	    ImageResource,
	    SoundResource
    }

    public class Core
    {
        /// <summary>
        /// Opens the graphical window so that it can be drawn onto. You can set the
	    ///	icon for this window using SetIcon. The window itself is only drawn when
	    ///	you call RefreshScreen. All windows are opened at 32 bits per pixel. You
	    ///	can toggle fullscreen using ToggleFullScreen. The window is closed when
	    ///	the application terminates.
        /// </summary>
        /// <param name="caption">Caption for the Window</param>
        /// <param name="width">Width of the Window</param>
        /// <param name="height">Height of the Window</param>
        [DllImport("SGSDK.dll")]
        public static extern void OpenGraphicsWindow(String caption, int width, int height);
        
        /// <summary>
        /// Checks to see if the window has been asked to close. You need to handle
        ///	this if you want the game to end when the window is closed. This value
        ///	is updated by the ProcessEvents routine. 
        /// </summary>
        /// <returns>Returns true if the window has been requested to close</returns>
        [DllImport("SGSDK.dll")]
        public static extern bool WindowCloseRequested();

        /// <summary>
        /// ProcessEvents allows the SwinGame API to react to user interactions. This
        ///	routine checks the current keyboard and mouse states. This routine must
        ///	be called frequently within your game loop to enable user interaction.
        /// </summary>
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
