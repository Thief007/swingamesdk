/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Point;

import swingame.emulator.EmulatedInput;
import swingame.platform.NativeInput;

/**
 *
 * @author acain
 */
public class Input 
{
    private static InputAdapter _ia = new EmulatedInput();
    
    protected static void useNative()
    {
        _ia = new NativeInput();
    }
    
    public static Point getMousePosition() 
    {
        return _ia.getMousePosition();
    }
    
    public static boolean mouseWasClicked(int button) { return _ia.mouseWasClicked(button); }
    public static boolean wasKeyTyped(int key) { return _ia.wasKeyTyped(key); }
    public static boolean isKeyPressed(int key) { return _ia.isKeyPressed(key); }
}
