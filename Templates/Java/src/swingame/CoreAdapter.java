package swingame;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public interface CoreAdapter 
{
    public void openGraphicsWindow(String caption, int width, int height);
    public void refreshScreen(int frameRate);
    public int screenHeight();
    public int screenWidth();
    public boolean windowCloseRequested();
    public void processEvents();
    public void refreshScreen();
    public void close();
}
