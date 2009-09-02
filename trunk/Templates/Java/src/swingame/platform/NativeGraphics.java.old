package swingame.platform;

import java.awt.Color;

import swingame.GraphicsAdapter;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class NativeGraphics implements GraphicsAdapter
{
    private static native void n_fillRectangle(int color, float x, float y, int w, int h);    
    public void fillRectangle(Color color, float x, float y, int w, int h) 
    {
        n_fillRectangle(color.getRGB(), x, y, w, h);
    }

    private static native void n_fillEllipse(int color, float x, float y, int w, int h);    
    public void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        n_fillEllipse(color.getRGB(), x, y, w, h);
    }

    
    private static native void n_drawPixel(int color, float x, float y);    
    public void drawPixel(Color color, float x, float y)
    {
        n_drawPixel(color.getRGB(), x, y);
    }
    
    private static native void n_drawLine(int color, float x, float y, float x1, float y1);
    public void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        n_drawLine(color.getRGB(), x, y, xEnd, yEnd);
    }
    
    private static native void n_clearScreen(int color);    
    public void clearScreen(Color color)
    {
        n_clearScreen(color.getRGB());
    }
}
