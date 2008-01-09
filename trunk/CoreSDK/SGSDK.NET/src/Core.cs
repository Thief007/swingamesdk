using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// SwinGameException
    /// </summary>
    public class SwinGameException : Exception
    {
        /// <summary>
        /// SwinGameException
        /// </summary>
        /// <param name="message">Exception Message</param>
        public SwinGameException(string message) : base(message)
        { }
    }
    /// <summary>
    /// ResourceKind
    ///
    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across platforms
    /// </summary>
    public enum ResourceKind
    {   
        /// <summary>
        /// Font Resource
        /// </summary>
	    FontResource,
        /// <summary>
        /// Image Resource
        /// </summary>
	    ImageResource,
        /// <summary>
        /// Sound Resource
        /// </summary>
	    SoundResource,
        /// <summary>
        /// Map Resource
        /// </summary>
        MapResource,
        /// <summary>
        /// No Resource
        /// </summary>
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
        
        [DllImport("SGSDK.dll")]
        private static extern int GetBitmapWidth(IntPtr pointer);
        [DllImport("SGSDK.dll")]
        private static extern int GetBitmapHeight(IntPtr pointer);

        /// <summary>
        /// The Width of the Bitmap
        /// </summary>
        public int Width
        {
            get { return Graphics.GetBitmapWidth(this); }
        }

        /// <summary>
        /// The Height of the Bitmap
        /// </summary>
        public int Height
        {
            get { return Graphics.GetBitmapHeight(this); }
        }
    }

    /// <summary>
    /// Vector Structure
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Vector
    {
        private Single X;
        private Single Y;
        private Single W;

        /// <summary>
        /// The X Component of the Vector
        /// </summary>
        public Single x
        {
            get { return X; }
            set { X = value; }
        }

        /// <summary>
        /// The Y Component of the Vecotr
        /// </summary>
        public Single y
        {
            get { return Y; }
            set { Y = value; }
        }

        /// <summary>
        /// The W Component of the Vector
        /// </summary>
        public Single w
        {
            get { return W; }
            set { W = value; }
        }
    }

    /// <summary>
    /// Core Class
    /// 
    /// This Classes contains on the Core functionality of the SwinGameSDK
    /// </summary>
    public class Core
    {
        // Debug Exception Code

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ExceptionOccured")]
        private static extern bool ExceptionOccured();
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetExceptionMessage")]
        public static extern String GetExceptionMessage();

        private static string _ErrorMessage;

        public static string Errormessage()
        {
            return _ErrorMessage;
        }
        internal static string ErrorMessage
        {
            set { value = _ErrorMessage; }
        }
        
        // Code

        [DllImport("SGSDK.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint="OpenGraphicsWindow")]
        private static extern void DLL_OpenGraphicsWindow(String caption, int width, int height);
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
        public static void OpenGraphicsWindow(String caption, int width, int height)
        {
            try
            {
                DLL_OpenGraphicsWindow(caption, width, height);
                if (ExceptionOccured())
                    throw new SwinGameException(GetExceptionMessage());
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }     
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "WindowCloseRequested")]
        private static extern bool DLL_WindowCloseRequested();
        /// <summary>
        /// Checks to see if the window has been asked to close. You need to handle
        ///	this if you want the game to end when the window is closed. This value
        ///	is updated by the ProcessEvents routine. 
        /// </summary>
        /// <returns>Returns true if the window has been requested to close</returns>
        public static bool WindowCloseRequested()
        {
            try
            {
                bool temp = DLL_WindowCloseRequested();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ProcessEvents")]
        private static extern void DLL_ProcessEvents();
        /// <summary>
        /// ProcessEvents allows the SwinGame API to react to user interactions. This
        ///	routine checks the current keyboard and mouse states. This routine must
        ///	be called frequently within your game loop to enable user interaction.
        /// </summary>
        public static void ProcessEvents()
        {
            DLL_ProcessEvents();
            try
            {
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="SetIcon")]
        private static extern void DLL_SetIcon(String iconFilename);
        /// <summary>
        /// Sets the icon for the window. This must be called before openning the
        ///	graphics window. The icon is loaded as a bitmap, though this can be from
        ///	any kind of bitmap file.
        /// </summary>
        /// <param name="iconFilename">The name of the file to load as the image icon</param>
        public static void SetIcon(String iconFilename)
        {
            try
            {
                DLL_SetIcon(iconFilename);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ChangeScreenSize")]
        private static extern void DLL_ChangeScreenSize(int width, int height);
        /// <summary>
        /// Changes the size of the screen.
        /// </summary>
        /// <param name="width">New width of the Screen</param>
        /// <param name="height">New height of the Screen</param>
        public static void ChangeScreenSize(int width, int height)
        {
            try
            {
                DLL_ChangeScreenSize(width, height);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ToggleFullScreen")]
        private static extern void DLL_ToggleFullScreen();
        /// <summary>
        /// Switches the application to full screen or back from full screen to
        ///	windowed.
        /// </summary>
        public static void ToggleFullScreen()
        {
            try
            {
                DLL_ToggleFullScreen();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="RefreshScreenWithFrame")]
        private static extern void DLL_RefreshScreenWithFrame(int TargetFPS);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "RefreshScreen")]
        private static extern void DLL_RefreshScreen();

        /// <summary>
        /// Draws the current drawing to the screen. This must be called to display
        ///	anything to the screen. This will draw all drawing operations, as well
        ///	as the text being entered by the user.
        /// </summary>
        /// <param name="TargetFPS">The target framerate</param>
        public static void RefreshScreen(int TargetFPS) 
        {
            try
            {
                DLL_RefreshScreenWithFrame(TargetFPS);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        /// <summary>
        /// Draws the current drawing to the screen. This must be called to display
        ///	anything to the screen. This will draw all drawing operations, as well
        ///	as the text being entered by the user.
        /// </summary>
        public static void RefreshScreen()
        {
            try
            {
                DLL_RefreshScreen();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="TakeScreenshot")]
        private static extern void DLL_TakeScreenshot(String basename);
        /// <summary>
        /// Saves the current screen a bitmap file. The file will be saved into the
        ///	current directory.
        /// </summary>
        /// <param name="basename">The base name for the screen shot</param>
        public static void TakeScreenshot(String basename)
        {
            try
            {
                DLL_TakeScreenshot(basename);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ScreenWidth")]
        private static extern int DLL_ScreenWidth();
        /// <summary>
        /// Gets the Screen's Width
        /// </summary>
        /// <returns>The Screen Width</returns>
        public static int ScreenWidth()
        {
            try
            {
                int temp = DLL_ScreenWidth();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
	
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ScreenHeight")]
        private static extern int DLL_ScreenHeight();
        /// <summary>
        /// Gets the Screen's Height
        /// </summary>
        /// <returns>The Screen Height</returns>
        public static int ScreenHeight()
        {
            try
            {
                int temp = DLL_ScreenHeight();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetColourRGBA")]
        private static extern int DLL_GetColor(Byte red, Byte green, Byte blue, Byte alpha);
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
            try
            {
                Color temp = Color.FromArgb(alpha, red, green, blue);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
            try
            {
                Color temp = Color.FromArgb(red, green, blue);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetFramerate")]
        private static extern int DLL_GetFramerate();
        /// <summary>
        /// Returns the average framerate for the last 10 frames as an integer.
        /// </summary>
        /// <returns>The current average framerate</returns>
        public static int GetFramerate()
        {
            try
            {
                int temp = DLL_GetFramerate();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetTicks")]
        private static extern UInt32 DLL_GetTicks();
        /// <summary>
        /// Gets the number of milliseconds that have passed. This can be used to
        ///	determine timing operations, such as updating the game elements.
        /// </summary>
        /// <returns>The number of milliseconds passed</returns>
        public static UInt32 GetTicks()
        {
            try
            {
                UInt32 temp = DLL_GetTicks();
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Sleep")]
        private static extern UInt32 DLL_Sleep(UInt32 time);
        /// <summary>
        /// /// Puts the process to sleep for a specified number of
        /// milliseconds. This can be used to add delays into your
        /// </summary>
        /// <param name="time">The number of milliseconds to sleep</param>
        /// <returns>Delay before returning</returns>
        public static UInt32 Sleep(UInt32 time)
        {
            try
            {
                UInt32 temp = DLL_Sleep(time);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResourceWithKind")]
        private static extern String DLL_GetPathToResourceWithKind(String filename, ResourceKind kind);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResource")]
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
            try
            {
                String temp = DLL_GetPathToResourceWithKind(filename, kind);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Gets the Path to a Resource in the base Resource folder.
        /// </summary>
        /// <param name="filename">filename that you need to get the path of</param>
        /// <returns>A Path to the Resource</returns>
        public static String GetPathToResource(String filename)
        {
            try
            {
                String temp = DLL_GetPathToResource(filename);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
	    
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Cos")]
        private static extern Single DLL_Cos(Single angle);
        /// <summary>
        /// Gets the Cos of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Cos</returns>
        public static Single Cos(Single angle)
        {
            try
            {
                Single temp = DLL_Cos(angle);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Sin")]
        private static extern Single DLL_Sin(Single angle);
        /// <summary>
        /// Gets the Sin of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Sin</returns>
        public static Single Sin(Single angle)
        {
            try
            {
                Single temp = DLL_Sin(angle);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Tan")]
        private static extern Single DLL_Tan(Single angle);
        /// <summary>
        /// Gets the Tan of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Tan</returns>
        public static Single Tan(Single angle)
        {
            try
            {
                Single temp = DLL_Tan(angle);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
    }
}
