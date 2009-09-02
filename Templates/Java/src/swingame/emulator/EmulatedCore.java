package swingame.emulator;

import java.awt.Color;
import java.awt.Graphics;

import swingame.CoreAdapter;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedCore implements CoreAdapter
{
    private static EmulatedWindow _window; 
    
    public void openGraphicsWindow(String caption, int width, int height)
    {
        //Create the window
        _window = new EmulatedWindow(caption, width, height);
        //remove initial backcolor - make black
        swingame.Graphics.clearScreen(Color.BLACK);
        //send to front buffer
        refreshScreen();
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
    
    public int screenHeight()
    {
        return _window.getHeight();
    }

    public int screenWidth()
    {
        return _window.getWidth();        
    }
    
    public boolean windowCloseRequested()
    {
        return false == _window.isVisible();
    }
    
    public void refreshScreen()
    {
        getWindow().refreshScreen();
    }

    public void refreshScreen(int fps)
    {
        getWindow().refreshScreen();
        try
        {
            java.lang.Thread.sleep(1000 / fps);
        } catch (Exception e){}
    }
    
    public void processEvents()
    {
        getWindow().processEvents();
    }
    
    public void close()
    {
        getWindow().setVisible(false);
    }
}
