package swingame.emulator;

import java.awt.Color;
import java.awt.Graphics;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedCore 
{
    private static EmulatedWindow _window; 
    
    public static void openGraphicsWindow(String caption, int width, int height)
    {
        //Create the window
        _window = new EmulatedWindow(caption, width, height);
        //remove initial backcolor - make black
        EmulatedGraphics.clearScreen(Color.BLACK);
        //send to front buffer
        EmulatedGraphics.refreshScreen();
        //draw window
        _window.setVisible(true);
    }
    
    protected static Graphics getGraphics()
    {
        return _window.getScreenGraphics();
    }
    
    protected static EmulatedWindow getWindow()
    {
        return _window;
    }
    
    public static int screenHeight()
    {
        return _window.getHeight();
    }

    public static int screenWidth()
    {
        return _window.getWidth();        
    }
    
}
