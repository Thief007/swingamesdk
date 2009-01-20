package swingame.platform;

import java.awt.Color;

import swingame.CoreAdapter;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class NativeCore implements CoreAdapter
{
    private static native void n_openGraphicsWindow(String caption, int width, int height);
    private static native void n_refreshScreen(int frameRate);
    private static native int n_screenHeight();
    private static native int n_screenWidth();
    private static native boolean n_windowCloseRequested();
    private static native void n_processEvents();
    private static native void n_refreshScreen();
    
    public void openGraphicsWindow(String caption, int width, int height) { n_openGraphicsWindow(caption, width, height); }
    public void refreshScreen(int frameRate) { n_refreshScreen(frameRate); }
    public int screenHeight() { return n_screenHeight(); }
    public int screenWidth() { return n_screenWidth(); }
    public boolean windowCloseRequested() { return n_windowCloseRequested(); }
    public void processEvents() { n_processEvents(); }
    public void refreshScreen() { n_refreshScreen(); }

    static
    {        
        try
        {
            System.loadLibrary("JavaSwinGame");
        }
        finally { System.out.println("Loaded SwinGame native library"); }
    }
        
    public void close()
    {}
}
