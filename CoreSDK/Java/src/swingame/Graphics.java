/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Color;

/**
 *
 * @author acain
 */
public class Graphics 
{
    private static native void fillRectangle(int color, float x, float y, int w, int h);    
    public static void fillRectangle(Color color, float x, float y, int w, int h) 
    {
        fillRectangle(color.getRGB(), x, y, w, h);
    }
    
    private static native void drawPixel(int color, float x, float y);    
    public static void drawPixel(Color color, float x, float y)
    {
        drawPixel(color.getRGB(), x, y);
    }
    
    private static native void drawLine(int color, float x, float y, float x1, float y1);
    public static void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        drawLine(color.getRGB(), x, y, xEnd, yEnd);
    }
    
    private static native void clearScreen(int color);    
    public static void clearScreen(Color color)
    {
        clearScreen(color.getRGB());
    }
    
    public static void clearScreen()
    {
        clearScreen(Color.BLACK);
    }
}
