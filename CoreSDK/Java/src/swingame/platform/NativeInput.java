/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.platform;

import java.awt.Point;

import swingame.InputAdapter;

/**
 *
 * @author acain
 */
public class NativeInput implements InputAdapter
{
    private static native void n_getMousePosition(Point p);    
    private static native boolean n_mouseWasClicked(int button);
    private static native boolean n_wasKeyTyped(int key);    
    private static native boolean n_isKeyPressed(int key);
    
    public Point getMousePosition() 
    {
        Point p = new Point();
        n_getMousePosition(p);        
        return p;
    }
    public boolean mouseWasClicked(int button) { return n_mouseWasClicked(button); }
    public boolean wasKeyTyped(int key) { return n_wasKeyTyped(key); }    
    public boolean isKeyPressed(int key) { return n_isKeyPressed(key); };
    
}
