package swingame;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class Core 
{
    //procedure OpenGraphicsWindow(caption : PChar; width : Integer; height : Integer); cdecl; export;
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
        //javax.swing.JFrame jf = new javax.swing.JFrame();
        
        openGraphicsWindow("Hello Java World", 800, 600);
        
        do
        {
            processEvents();
            refreshScreen();
        } while ( false == windowCloseRequested());
        
        //jf.setVisible(true);
    }
}
