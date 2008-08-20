package swingame.emulator;

import java.awt.Graphics;
import java.awt.Color;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedGraphics 
{
    public static void clearScreen(Color color)
    {
        Graphics g = EmulatedCore.getGraphics();
        g.setColor(color);
        g.fillRect(0, 0, 800, 600); //EmulatedCore.screenWidth(), EmulatedCore.screenHeight());
    }
    
    public static void refreshScreen()
    {
        EmulatedCore.getWindow().refreshScreen();
    }
}
