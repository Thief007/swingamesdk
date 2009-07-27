/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Color;

import swingame.emulator.EmulatedGraphics;
// import swingame.platform.NativeGraphics;

/**
 *
 * @author acain
 */
public class Graphics 
{
    private static GraphicsAdapter _ga = new EmulatedGraphics();
    
    // protected static void useNative()
    // {
    //     _ga = new NativeGraphics();
    // }
    
    public static void fillRectangle(Color color, float x, float y, int w, int h) 
    {
        _ga.fillRectangle(color, x, y, w, h);
    }

    public static void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        _ga.fillEllipse(color, x, y, w, h);
    }
    
    public static void fillCircle(Color color, float x, float y, int r) 
    {
        _ga.fillEllipse(color, x - r, y - r, w + 2 * r, h + 2 * r);
    }
    
    public static void drawPixel(Color color, float x, float y)
    {
        _ga.drawPixel(color, x, y);
    }
    
    public static void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        _ga.drawLine(color, x, y, xEnd, yEnd);
    }
    
    public static void clearScreen(Color color)
    {
        _ga.clearScreen(color);
    }
    
    public static void clearScreen()
    {
        clearScreen(Color.BLACK);
    }
}
