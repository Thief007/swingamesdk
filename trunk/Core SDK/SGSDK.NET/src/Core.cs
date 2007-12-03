using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;


namespace SwinGame
{
    /// <summary>
    /// ResourceKind
    ///
    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across platforms
    /// </summary>
    public enum ResourceKind
    {     
	    FontResource,
	    ImageResource,
	    SoundResource,
        None
    }

    /// <summary>
    /// Bitmap
    ///
    ///	The bitmap type is a pointer to a BitmapData. The BitmapData record
    ///	contains the data used by the SwinGame API to represent
    ///	bitmaps. You can create new bitmaps in memory for drawing operatings
    ///	using the CreateBitmap function. This can then be optimised for drawing
    ///	to the screen using the OptimiseBitmap routine. Also see the DrawBitmap
    ///	routines.
    /// </summary>
    public struct Bitmap
    {
        internal IntPtr pointer;
        
        [DllImport("lib/SGSDK.dll")]
        private static extern int GetBitmapWidth(IntPtr pointer);
        [DllImport("lib/SGSDK.dll")]
        private static extern int GetBitmapHeight(IntPtr pointer);

        public int Width
        {
            get { return GetBitmapWidth(pointer); }
        }

        public int Height
        {
            get { return GetBitmapHeight(pointer); }
        }
    }

    /// <summary>
    /// Vector Structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Ansi)]
    public struct Vector
    {
        Single x;
        Single y;
        Single w;

        public Single X
        {
            get { return x; }
        }

        public Single Y
        {
            get { return y; }
        }

        public Single W
        {
            get { return w; }
        }
        
    }

    /// <summary>
    /// Core Class
    /// 
    /// This Classes contains on the Core functionality of the SwinGameSDK
    /// </summary>
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
        [DllImport("lib/SGSDK.dll", CallingConvention=CallingConvention.Cdecl)]
        public static extern void OpenGraphicsWindow(String caption, int width, int height);
        
        /// <summary>
        /// Checks to see if the window has been asked to close. You need to handle
        ///	this if you want the game to end when the window is closed. This value
        ///	is updated by the ProcessEvents routine. 
        /// </summary>
        /// <returns>Returns true if the window has been requested to close</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern bool WindowCloseRequested();

        /// <summary>
        /// ProcessEvents allows the SwinGame API to react to user interactions. This
        ///	routine checks the current keyboard and mouse states. This routine must
        ///	be called frequently within your game loop to enable user interaction.
        /// </summary>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ProcessEvents();

        /// <summary>
        /// Sets the icon for the window. This must be called before openning the
        ///	graphics window. The icon is loaded as a bitmap, though this can be from
        ///	any kind of bitmap file.
        /// </summary>
        /// <param name="iconFilename">The name of the file to load as the image icon</param>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void SetIcon(String iconFilename);

        /// <summary>
        /// Changes the size of the screen.
        /// </summary>
        /// <param name="width">New width of the Screen</param>
        /// <param name="height">New height of the Screen</param>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ChangeScreenSize(int width, int height);

        /// <summary>
        /// Switches the application to full screen or back from full screen to
        ///	windowed.
        /// </summary>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ToggleFullScreen();

        /// <summary>
        /// Draws the current drawing to the screen. This must be called to display
        ///	anything to the screen. This will draw all drawing operations, as well
        ///	as the text being entered by the user.
        /// </summary>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void RefreshScreen();
	
        /// <summary>
        /// Saves the current screen a bitmap file. The file will be saved into the
        ///	current directory.
        /// <param name="basename">The base name for the screen shot</param>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void TakeScreenshot(String basename);

        /// <summary>
        /// Gets the Screen's Width
        /// </summary>
        /// <returns>The Screen Width</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenWidth();
	
        /// <summary>
        /// Gets the Screen's Height
        /// </summary>
        /// <returns>The Screen Height</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenHeight();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetColourRGBA")]
        private static extern int DLL_GetColor(Byte red, Byte green, Byte blue, Byte alpha);

        //function	GetColour(forBitmap: Bitmap; apiColor: Color): Colour; overload; cdecl; export;
        //begin
        //result := SGSDK_Core.GetColour(forBitmap, apiColor);
        //end;

        /// <summary>
        /// Gets the Color when the user enters the amount of red, green, blue and alpha
        /// </summary>
        /// <param name="red">The amount of red (0 - 255)</param>
        /// <param name="green">The amount of green (0 - 255)</param>
        /// <param name="blue">The amount of blue (0 - 255)</param>
        /// <param name="alpha">The amount of alpha (0 - 255)</param>
        /// <returns>Color</returns>
        public static Color GetColor(Byte red, Byte green, Byte blue, Byte alpha)
        {
            return Color.FromArgb(alpha, red, green, blue);
        }

        /// <summary>
        /// Gets the Color when the user enters the amount of red, green and blue
        /// </summary>
        /// <param name="red">The amount of red (0 - 255)</param>
        /// <param name="green">The amount of green (0 - 255)</param>
        /// <param name="blue">The amount of blue (0 - 255)</param>
        /// <returns>Color</returns>
        public static Color GetColor(Byte red, Byte green, Byte blue)
        {
            return Color.FromArgb(red, green, blue);
        }
	    /// <summary>
        /// Returns the average framerate for the last 10 frames as an integer.
	    /// </summary>
	    /// <returns>The current average framerate</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int GetFramerate();
	    
        /// <summary>
        /// Gets the number of milliseconds that have passed. This can be used to
        ///	determine timing operations, such as updating the game elements.
        /// </summary>
        /// <returns>The number of milliseconds passed</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt32 GetTicks();

        /// <summary>
        /// /// Puts the process to sleep for a specified number of
        /// milliseconds. This can be used to add delays into your
        /// </summary>
        /// <param name="time">The number of milliseconds to sleep</param>
        /// <returns>Delay before returning</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern UInt32 Sleep(UInt32 time);


        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResourceWithKind")]
        private static extern String DLL_GetPathToResourceWithKind(String filename, ResourceKind kind);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResource")]
        private static extern String DLL_GetPathToResource(String filename);

        /// <summary>
        /// Gets the resource to an image, sound, font or other type of resource
        /// 
        /// Entering ResourceKind.None into the kind parameters makes this function
        /// look inside the base resource folder, while entering either, font, image
        /// or sound, will make this function look inside their respective folders, 
        /// image, font and sound folders.
        /// </summary>
        /// <param name="filename">filename that you need to get the path of</param>
        /// <param name="kind">The type of resource it is</param>
        /// <returns>A Path to the Resource</returns>
        public static String GetPathToResource(String filename, ResourceKind kind)
        {
            return DLL_GetPathToResourceWithKind(filename, kind);
        }

        public static String GetPathToResource(String filename)
        {
            return DLL_GetPathToResource(filename);
        }
	    
        /// <summary>
        /// Gets the Cos of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Cos</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern Single Cos(Single angle);

        /// <summary>
        /// Gets the Sin of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Sin</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern Single Sin(Single angle);

        /// <summary>
        /// Gets the Tan of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Tan</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern Single Tan(Single angle);
     }
}
