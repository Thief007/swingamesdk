package swingame;

import java.awt.Point;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public interface InputAdapter 
{
    public Point getMousePosition();
    public boolean mouseWasClicked(int button);
    public boolean wasKeyTyped(int key);    
    public boolean isKeyPressed(int key);
    
}
