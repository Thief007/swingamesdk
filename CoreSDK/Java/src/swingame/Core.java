package swingame;

import java.awt.Color;

import swingame.emulator.EmulatedCore;
import swingame.platform.NativeCore;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class Core 
{
    private static CoreAdapter _ca = new EmulatedCore();
    
    public static void useNative()
    {
        _ca = new NativeCore();
        Graphics.useNative();
        Input.useNative();
    }
    
    public static void openGraphicsWindow(String caption, int width, int height) { _ca.openGraphicsWindow(caption, width, height); }
    public static void refreshScreen(int frameRate) { _ca.refreshScreen(frameRate); }
    public static int screenHeight() { return _ca.screenHeight(); }
    public static int screenWidth() { return _ca.screenWidth(); }
    public static boolean windowCloseRequested() { return _ca.windowCloseRequested(); }
    public static void processEvents() { _ca.processEvents(); }
    public static void refreshScreen() { _ca.refreshScreen(); }
    
    public static Color getHSBColor(float hue, float saturation, float brightness)
    {
        return Color.getHSBColor(hue, saturation, brightness);
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
    
    public static void close()
    {
        _ca.close();
    }
}