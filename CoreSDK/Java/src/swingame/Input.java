/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Point;

/**
 *
 * @author acain
 */
public class Input 
{
    public static native void getMousePosition(Point p);
    public static Point getMousePosition() 
    {
        Point p = new Point();
        getMousePosition(p);        
        return p;
    }
    
    public static native boolean mouseWasClicked(int LEFT_BUTTON);
    public static native boolean wasKeyTyped(int key);    
}
