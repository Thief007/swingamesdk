package swingame.emulator;

import java.awt.Graphics;
import java.awt.Color;

import swingame.Core;
import swingame.GraphicsAdapter;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedGraphics implements GraphicsAdapter
{
    public void clearScreen(Color color)
    {
        Graphics g = EmulatedCore.getGraphics();
        g.setColor(color);
        g.fillRect(0, 0, Core.screenWidth(), Core.screenHeight());
    }
    
    public void fillRectangle(Color color, float x, float y, int w, int h)
    {
        Graphics g = EmulatedCore.getGraphics();
        g.setColor(color);
        g.fillRect((int)x, (int)y, w, h);
    }
    
    public void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        Graphics g = EmulatedCore.getGraphics();
        g.setColor(color);
        g.fillOval((int)x, (int)y, w, h);        
    }
    
    public void drawPixel(Color color, float x, float y)
    {
        fillRectangle(color, x, y, 1, 1);        
    }
    
    public void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        Graphics g = EmulatedCore.getGraphics();
        g.setColor(color);
        g.drawLine((int)x, (int)y, (int)xEnd, (int)yEnd);                
    }
}
