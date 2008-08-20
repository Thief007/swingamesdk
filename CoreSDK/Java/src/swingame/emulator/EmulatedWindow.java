package swingame.emulator;

import java.awt.Image;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedWindow extends Frame
{
    private Image _backImage;
    private Image _nextImage;
    
    public EmulatedWindow(String caption, int width, int height) 
    {
        Insets insets = getInsets();
        
        setTitle(caption);
        setResizable(false);
        
        int totalWidth = width + (insets.left + insets.right);
        int totalHeight = height + (insets.top + insets.bottom);
        setSize(totalWidth, totalHeight);
        
        _backImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
        _nextImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
                
        addWindowListener ( new  WindowAdapter() {
                public void windowClosing(WindowEvent e) 
                {
                    System.exit(0);
                }
            });
    }
    
    protected Graphics getScreenGraphics()
    {
        return _nextImage.getGraphics();
    }
    
    public void refreshScreen()
    {
        Graphics g = _backImage.getGraphics();
        g.drawImage(_nextImage, 0, 0, null);
    }
    
    public void paint(Graphics g)
    {
        //g.setColor(Color.BLACK);
        //g.fillRect(0, 0, getWidth(), getHeight());
        
        g.drawImage(_backImage, 0, 0, null);
    }
}
