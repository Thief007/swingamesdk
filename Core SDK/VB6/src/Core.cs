using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGameVB
{

    /// <summary>
    /// ResourceKind
    ///
    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across platforms
    /// </summary>
    [ComVisible(true)]
    public enum ResourceKind
    {
        FontResource,
        ImageResource,
        SoundResource,
        None
    }

    [Guid("7EBEB570-22AE-4b09-9E56-9A3B9FD4FA57")]
    [ComVisible(true)]
    public interface IBitmap
    {
        int Width();
        int Height();
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
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("6939978A-4BD4-4c0f-94DC-ED66F9C359A1")]
    [ComVisible(true)]
    public class Bitmap :IBitmap
    {
        private SwinGame.Bitmap bitmap = new SwinGame.Bitmap();

        internal SwinGame.Bitmap result()
        {
            return bitmap;
        }


        public int Width()
        {
            return bitmap.Width; 
        }

        public int Height()
        {
            return bitmap.Height;
        }
    }

    [Guid("35EC3022-5CFA-4446-8FD8-8C33C79055BE")]
    [ComVisible(true)]
    public interface IVector
    {
        Single getX();
        void setX(Single value);
        Single getY();
        void setY(Single value);
        Single getW();
        void setW(Single value);

    }

    /// <summary>
    /// Vector Structure
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("EAFEC7C8-CAEB-4c00-9E45-EFF91F7DED5C")]
    [ComVisible(true)]
    public class Vector : IVector
    {
        private SwinGame.Vector vector = new SwinGame.Vector();


        public Single getX()
        {
            return vector.x;
        }
        public void setX(Single value)
        {
            vector.x = value;
        }
        public Single getY()
        {
            return vector.y;
        }
        public void setY(Single value)
        {
            vector.y = value;
        }
        public Single getW()
        {
            return vector.w;
        }
        public void setW(Single value)
        {
            vector.w = value;
        }

        internal SwinGame.Vector result()
        {
            return vector;
        }
    }

    [Guid("430227DE-A525-4880-9B6A-5A1634A10200")]
    [ComVisible(true)]
    public interface ICore
    {
        string OpenGraphicsWindow(String caption, int width, int height);
        bool WindowCloseRequested();
        void ProcessEvents();
        void SetIcon(String iconFilename);
        void ChangeScreenSize(int width, int height);
        void ToggleFullScreen();
        void RefreshScreen();
        void TakeScreenshot(String basename);
        int ScreenWidth();
        int ScreenHeight();
        Color GetColor(Byte red, Byte green, Byte blue, Byte alpha);
        Color GetColor(Byte red, Byte green, Byte blue);
        int GetFramerate();
        int GetTicks();
        int Sleep(int time);
        String GetPathToResource(String filename, ResourceKind kind);
        String GetPathToResource(String filename);
        Single Cos(Single angle);
        Single Sin(Single angle);
        Single Tan(Single angle);
    }

    /// <summary>
        /// Core Class
        /// 
        /// This Classes contains on the Core functionality of the SwinGameSDK
        /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("696A5E07-D52D-4ee7-A2F9-B2387D17B56F")]
    [ComVisible(true)]
    public class Core :ICore
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
            public string OpenGraphicsWindow(String caption, int width, int height)
            {
                SwinGame.Core.OpenGraphicsWindow(caption, width, height);
                return caption;
            }

            /// <summary>
            /// Checks to see if the window has been asked to close. You need to handle
            ///	this if you want the game to end when the window is closed. This value
            ///	is updated by the ProcessEvents routine. 
            /// </summary>
            /// <returns>Returns true if the window has been requested to close</returns>
            public bool WindowCloseRequested()
            {
                return SwinGame.Core.WindowCloseRequested();
            }

            /// <summary>
            /// ProcessEvents allows the SwinGame API to react to user interactions. This
            ///	routine checks the current keyboard and mouse states. This routine must
            ///	be called frequently within your game loop to enable user interaction.
            /// </summary>
            public void ProcessEvents()
            {
                SwinGame.Core.ProcessEvents();
            }

            /// <summary>
            /// Sets the icon for the window. This must be called before openning the
            ///	graphics window. The icon is loaded as a bitmap, though this can be from
            ///	any kind of bitmap file.
            /// </summary>
            /// <param name="iconFilename">The name of the file to load as the image icon</param>
            public void SetIcon(String iconFilename)
            {
                SwinGame.Core.SetIcon(iconFilename);
            }

            /// <summary>
            /// Changes the size of the screen.
            /// </summary>
            /// <param name="width">New width of the Screen</param>
            /// <param name="height">New height of the Screen</param>
            public void ChangeScreenSize(int width, int height)
            {
                SwinGame.Core.ChangeScreenSize(width, height);
            }

            /// <summary>
            /// Switches the application to full screen or back from full screen to
            ///	windowed.
            /// </summary>
            public void ToggleFullScreen()
            {
                SwinGame.Core.ToggleFullScreen();
            }

            /// <summary>
            /// Draws the current drawing to the screen. This must be called to display
            ///	anything to the screen. This will draw all drawing operations, as well
            ///	as the text being entered by the user.
            /// </summary>
            public void RefreshScreen()
            {
                SwinGame.Core.RefreshScreen();
            }

            /// <summary>
            /// Saves the current screen a bitmap file. The file will be saved into the
            ///	current directory.
            /// <param name="basename">The base name for the screen shot</param>
            public void TakeScreenshot(String basename)
            {
                SwinGame.Core.TakeScreenshot(basename);
            }

            /// <summary>
            /// Gets the Screen's Width
            /// </summary>
            /// <returns>The Screen Width</returns>
            public int ScreenWidth()
            {
                return SwinGame.Core.ScreenWidth();
            }

            /// <summary>
            /// Gets the Screen's Height
            /// </summary>
            /// <returns>The Screen Height</returns>
            public int ScreenHeight()
            {
                return SwinGame.Core.ScreenHeight();
            }

            /// <summary>
            /// Gets the Color when the user enters the amount of red, green, blue and alpha
            /// </summary>
            /// <param name="red">The amount of red (0 - 255)</param>
            /// <param name="green">The amount of green (0 - 255)</param>
            /// <param name="blue">The amount of blue (0 - 255)</param>
            /// <param name="alpha">The amount of alpha (0 - 255)</param>
            /// <returns>Color</returns>
            public Color GetColor(Byte red, Byte green, Byte blue, Byte alpha)
            {
                return SwinGame.Core.GetColor( red, green, blue, alpha);
            }

            /// <summary>
            /// Gets the Color when the user enters the amount of red, green and blue
            /// </summary>
            /// <param name="red">The amount of red (0 - 255)</param>
            /// <param name="green">The amount of green (0 - 255)</param>
            /// <param name="blue">The amount of blue (0 - 255)</param>
            /// <returns>Color</returns>
            public Color GetColor(Byte red, Byte green, Byte blue)
            {
                return SwinGame.Core.GetColor(red, green, blue);
            }
            /// <summary>
            /// Returns the average framerate for the last 10 frames as an integer.
            /// </summary>
            /// <returns>The current average framerate</returns>
            public int GetFramerate()
            {
                return SwinGame.Core.GetFramerate();
            }

            /// <summary>
            /// Gets the number of milliseconds that have passed. This can be used to
            ///	determine timing operations, such as updating the game elements.
            /// </summary>
            /// <returns>The number of milliseconds passed</returns>
            public int GetTicks()
            {

                return (int)SwinGame.Core.GetTicks();
            }

            /// <summary>
            /// /// Puts the process to sleep for a specified number of
            /// milliseconds. This can be used to add delays into your
            /// </summary>
            /// <param name="time">The number of milliseconds to sleep</param>
            /// <returns>Delay before returning</returns>
            public int Sleep(int time)
            {
                return (int)SwinGame.Core.Sleep((UInt32)time);
            }

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
            public String GetPathToResource(String filename, ResourceKind kind)
            {
                return SwinGame.Core.GetPathToResource(filename, (SwinGame.ResourceKind)kind);
            }

            public String GetPathToResource(String filename)
            {
                return SwinGame.Core.GetPathToResource(filename);
            }

            /// <summary>
            /// Gets the Cos of an angle
            /// </summary>
            /// <param name="angle">Angle</param>
            /// <returns>Cos</returns>
            public Single Cos(Single angle)
            {
                return SwinGame.Core.Cos(angle);
            }

            /// <summary>
            /// Gets the Sin of an angle
            /// </summary>
            /// <param name="angle">Angle</param>
            /// <returns>Sin</returns>
            public Single Sin(Single angle)
            {
                return SwinGame.Core.Sin(angle);
            }

            /// <summary>
            /// Gets the Tan of an angle
            /// </summary>
            /// <param name="angle">Angle</param>
            /// <returns>Tan</returns>
            public Single Tan(Single angle)
            {
                return SwinGame.Core.Tan(angle);
            }
        
    }

}