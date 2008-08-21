/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.emulator;

import java.awt.Point;

import swingame.InputAdapter;

/**
 *
 * @author acain
 */
public class EmulatedInput implements InputAdapter
{
    public Point getMousePosition() 
    {
        return EmulatedCore.getWindow().getMousePoint();
    }
    
    public boolean mouseWasClicked(int button) 
    { 
        return EmulatedCore.getWindow().mouseWasClicked(button); 
    }
    
    public boolean wasKeyTyped(int key) 
    {
        return EmulatedCore.getWindow().keyTyped(key); 
    }
    
    public boolean isKeyPressed(int key) 
    { 
        return EmulatedCore.getWindow().keyDown(key); 
    }
}
