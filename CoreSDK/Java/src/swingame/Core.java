package swingame;

import java.awt.Color;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class Core 
{
    public static native void openGraphicsWindow(String caption, int width, int height);
    public static native void refreshScreen(int frameRate);

    public static native int screenHeight();

    public static native int screenWidth();
    
    //function WindowCloseRequested(): Integer; cdecl; export;
    public static native boolean windowCloseRequested();
    
    //procedure ProcessEvents(); cdecl; export;
    public static native void processEvents();
    
    //procedure RefreshScreen(); cdecl; export;
    public static native void refreshScreen();
    
    public static Color getHSBColor(float hue, float saturation, float brightness)
    {
        return Color.getHSBColor(hue, saturation, brightness);
    }
    
    static
    {        
        try
        {
            System.loadLibrary("JavaSwinGame");
        }
        finally { System.out.println("Loaded it... really"); }
    }
    
    public static void main(String args[])
    {
        openGraphicsWindow("Hello Java World", 800, 600);
        
        do
        {
            processEvents();
            refreshScreen();
        } while ( false == windowCloseRequested());
    }
    
    /**
    * Returns the hue component of the passed in color.
    *
    * @param c  The color to get the Hue of
    * @return The hue of the color c
    */
    public static float getHue(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[0];
    }
    
    /**
    * Returns the saturation component of the passed in color.
    *
    * @param c  The color to get the saturation of
    * @return The saturation of the color c
    */
    public static float getSaturation(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[1];
    }
    
    /**
    * Returns the brightness component of the passed in color.
    *
    * @param c  The color to get the Brightness of
    * @return The brightness of the color c
    */
    public static float getBrightness(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[2];
    }
}
