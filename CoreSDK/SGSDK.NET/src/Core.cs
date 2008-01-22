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

	public struct Timer
	{
		internal IntPtr Pointer;
	}

    /// <summary>
    /// Core Class
    /// 
    /// This Classes contains on the Core functionality of the SwinGameSDK
    /// </summary>
    public class Core
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ExceptionOccured")]
        private static extern int DLL_ExceptionOccured();

        /// <summary>
        /// Indicates if an exception has occurred in the SwinGame Library. This is used to determine the error
        /// The error message to be returned to the user.
        /// </summary>
        /// <returns>True if an error has occurred</returns>
        internal static bool ExceptionOccured() { return DLL_ExceptionOccured() == -1; }
        
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetExceptionMessage")]
        private static extern IntPtr DLL_GetExceptionMessage();
        
        internal static string GetExceptionMessage()
        {
           return Marshal.PtrToStringAnsi(DLL_GetExceptionMessage());
        }

        // Code

		#region "OS X compatibility"
        [DllImport("/System/Library/Frameworks/Cocoa.framework/Cocoa", EntryPoint = "NSApplicationLoad")]
        private static extern void NSApplicationLoad();

        [DllImport("libobjc.dylib", EntryPoint = "objc_getClass")]
        private static extern int objc_getClass(string name);
        
        [DllImport("libobjc.dylib", EntryPoint = "sel_registerName")]
        private static extern int sel_registerName(string name);

        [DllImport("libobjc.dylib", EntryPoint = "objc_msgSend")]
        private static extern int objc_msgSend(int self, int cmd);
		#endregion

        [DllImport("SGSDK.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint="OpenGraphicsWindow")]
        private static extern void DLL_OpenGraphicsWindow([MarshalAs(UnmanagedType.LPStr)]String caption, int width, int height);
        
        /// <summary>
        /// Opens the graphical window so that it can be drawn onto. You can set the
        ///	icon for this window using SetIcon. The window itself is only drawn when
        ///	you call RefreshScreen. All windows are opened at 32 bits per pixel. You
        ///	can toggle fullscreen using ToggleFullScreen. The window is closed when
        ///	the application terminates. Width and Height are set to 800 by 600.
        /// </summary>
        /// <param name="caption">Caption for the Window</param>
		public static void OpenGraphicsWindow(string caption)
		{
			OpenGraphicsWindow(caption, 800, 600);	
		}

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
					//Console.WriteLine("Openning GW");
				     ////Mac OSX code
				     if (File.Exists("/System/Library/Frameworks/Cocoa.framework/Cocoa"))
				     {
							//Console.WriteLine("Loading Mac version");
				         int NSAutoreleasePool = objc_getClass("NSAutoreleasePool");
				         objc_msgSend(NSAutoreleasePool, sel_registerName("new"));
				    		NSApplicationLoad();
				     }
				 }
				 catch(Exception exc)
				 {
					Console.WriteLine("Error loading Mac: " + exc.Message);
				 }

            try
            {
                DLL_OpenGraphicsWindow(caption, width, height);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }     

            if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "WindowCloseRequested")]
        private static extern int DLL_WindowCloseRequested();

        /// <summary>
        /// Checks to see if the window has been asked to close. You need to handle
        ///	this if you want the game to end when the window is closed. This value
        ///	is updated by the ProcessEvents routine. 
        /// </summary>
        /// <returns>Returns true if the window has been requested to close</returns>
        public static bool WindowCloseRequested()
        {
				bool temp;
            try
            {
                temp = DLL_WindowCloseRequested() == -1;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
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
            try
            {
	            DLL_ProcessEvents();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="SetIcon")]
        private static extern void DLL_SetIcon([MarshalAs(UnmanagedType.LPStr)]String iconFilename);
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="TakeScreenshot")]
        private static extern void DLL_TakeScreenshot([MarshalAs(UnmanagedType.LPStr)]String basename);
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
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
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
				int temp;
            try
            {
                temp = DLL_ScreenWidth();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
	
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ScreenHeight")]
        private static extern int DLL_ScreenHeight();
        /// <summary>
        /// Gets the Screen's Height
        /// </summary>
        /// <returns>The Screen Height</returns>
        public static int ScreenHeight()
        {
				int temp;
            try
            {
                temp = DLL_ScreenHeight();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetColourRGBA")]
        //private static extern int DLL_GetColor(Byte red, Byte green, Byte blue, Byte alpha);
        
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
				Color temp = Color.FromArgb(alpha, red, green, blue);
            return temp;
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
				return GetColor(red, green, blue, 255);
        }

			public static Color GetRGBFloatColor(float red, float green, float blue)
			{
				return GetColor((byte)(red * 255), (byte)(green * 255), (byte)(blue * 255));
			}

			public static Color GetHSBColor(float hue, float saturation, float brightness)
			{
				float domainOffset;
				float red, green, blue;

				if (brightness == 0) 
				{
					return Color.Black;
				}

				if (saturation == 0)
				{
					return GetRGBFloatColor(brightness, brightness, brightness);
				}
				
				if (hue < 1.0 / 6 )
				{
					//Red domain... green ascends
					domainOffset = hue;
					red 			 = brightness;
					blue  		 = brightness * (1.0f - saturation);
		        	green 		 = blue + (brightness - blue) * domainOffset * 6;
				}				
				else if (hue < 2.0 / 6)
				{
					// yellow domain; red descends
		        	domainOffset = hue - 1.0f / 6;
			      green = brightness;
		    	   blue  = brightness * (1.0f - saturation);
		        	red   = green - (brightness - blue) * domainOffset * 6;
				}
				else if (hue < 3.0 / 6)
				{
		      	// green domain; blue ascends
		        	domainOffset = hue - 2.0f / 6;
		        	green = brightness;
		        	red   = brightness * (1.0f - saturation);
		        	blue  = red + (brightness - red) * domainOffset * 6;
		      }
			   else if (hue < 4.0 / 6)
				{
		      		// cyan domain; green descends
		        	domainOffset = hue - 3.0f / 6;
					blue  = brightness;
		    	   red   = brightness * (1.0f - saturation);
		        	green = blue - (brightness - red) * domainOffset * 6;
			    }
		       else if (hue < 5.0 / 6)
		       {
		       		// blue domain; red ascends
		        	domainOffset = hue - 4.0f / 6;
		        	blue  = brightness;
		        	green = brightness * (1.0f - saturation);
		        	red   = green + (brightness - green) * domainOffset * 6;
		      }
		      else
		      {
					// magenta domain; blue descends
					domainOffset = hue - 5.0f / 6;
					red   = brightness;
					green = brightness * (1.0f - saturation);
					blue  = red - (brightness - green) * domainOffset * 6;
		      }

		      return GetRGBFloatColor(red, green, blue);
		}

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetFramerate")]
        private static extern int DLL_GetFramerate();
        /// <summary>
        /// Returns the average framerate for the last 10 frames as an integer.
        /// </summary>
        /// <returns>The current average framerate</returns>
        public static int GetFramerate()
        {
				int temp;
            try
            {
                temp = DLL_GetFramerate();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
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
				UInt32 temp;
            try
            {
                temp = DLL_GetTicks();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
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
				UInt32 temp;
            try
            {
                temp = DLL_Sleep(time);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResourceWithBaseAndKind")]
        private static extern IntPtr DLL_GetPathToResourceWithBaseAndKind([MarshalAs(UnmanagedType.LPStr)]string path, [MarshalAs(UnmanagedType.LPStr)]string filename, ResourceKind kind);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPathToResourceWithBase")]
        private static extern IntPtr DLL_GetPathToResourceWithBase([MarshalAs(UnmanagedType.LPStr)]string path, [MarshalAs(UnmanagedType.LPStr)]string filename);

			private static readonly string appPath;

			static Core()
			{
				appPath = System.Reflection.Assembly.GetExecutingAssembly().Location;
				appPath = System.IO.Path.GetDirectoryName(appPath);
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
        public static String GetPathToResource(String filename, ResourceKind kind)
        {
				string temp;
				try
				{
					IntPtr addr = DLL_GetPathToResourceWithBaseAndKind(appPath, filename, kind);
					temp = Marshal.PtrToStringAnsi(addr);
				}
				catch (Exception)
				{
					throw new SwinGameException(Core.GetExceptionMessage());
				}  
				if (Core.ExceptionOccured())
				{
					throw new SwinGameException(Core.GetExceptionMessage());
				}
				return temp;
        }

        /// <summary>
        /// Gets the Path to a Resource in the base Resource folder.
        /// </summary>
        /// <param name="filename">filename that you need to get the path of</param>
        /// <returns>A Path to the Resource</returns>
        public static String GetPathToResource(String filename)
        {
				string temp;
            try
            {
                temp = Marshal.PtrToStringAnsi(DLL_GetPathToResourceWithBase(appPath, filename));
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
	    
//        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Cos")]
//        private static extern Single DLL_Cos(Single angle);

		private const float DEG_TO_RAD = 0.0174532925f;

        /// <summary>
        /// Gets the Cos of an angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <returns>Cos</returns>
        public static Single Cos(Single angle)
        {
				return (float)Math.Cos(angle * DEG_TO_RAD);
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
				return (float)Math.Sin(angle * DEG_TO_RAD);
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
				return (float)Math.Tan(angle * DEG_TO_RAD);
        }

		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="CreateTimer")]
		public static extern IntPtr DLL_CreateTimer();
		
		public static Timer CreateTimer()
		{
			Timer temp;
         try
         {
             temp.Pointer = DLL_CreateTimer();
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }
         return temp;			
		}

		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="FreeTimer")]
		public static extern void DLL_FreeTimer(ref IntPtr timer);
		
		public static void FreeTimer(ref Timer toFree)
		{
         try
         {
             DLL_FreeTimer(ref toFree.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }			
		}
		
		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="StartTimer")]
		public static extern void DLL_StartTimer(IntPtr timer);
		
		public static void StartTimer(Timer toStart)
		{
         try
         {
             DLL_StartTimer(toStart.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }			
		}

		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="StopTimer")]
		public static extern void DLL_StopTimer(IntPtr timer);
		
		public static void StopTimer(Timer toStop)
		{
         try
         {
             DLL_StopTimer(toStop.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }			
		}

		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="PauseTimer")]
		public static extern void DLL_PauseTimer(IntPtr timer);
		
		public static void PauseTimer(Timer toPause)
		{
         try
         {
             DLL_PauseTimer(toPause.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }			
		}
		
		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="UnpauseTimer")]
		public static extern void DLL_UnpauseTimer(IntPtr timer);
		
		public static void UnpauseTimer(Timer toUnpause)
		{
         try
         {
             DLL_UnpauseTimer(toUnpause.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }			
		}

		[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetTimerTicks")]
		public static extern UInt32 DLL_GetTimerTicks(IntPtr timer);
		
		public static UInt32 GetTimerTicks(Timer toGet)
		{
			UInt32 temp;
         try
         {
             temp = DLL_GetTimerTicks(toGet.Pointer);
         }
         catch (Exception exc)
         {
             throw new SwinGameException(exc.Message);
         }  
         if (Core.ExceptionOccured())
         {
             throw new SwinGameException(Core.GetExceptionMessage());
         }
         return temp;			
		}
    }
}