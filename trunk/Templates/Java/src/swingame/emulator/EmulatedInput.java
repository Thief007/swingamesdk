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
    public Point mousePosition() 
    {
        return EmulatedCore.getWindow().getMousePoint();
    }
    
    public boolean mouseClicked(int button) 
    { 
        return EmulatedCore.getWindow().mouseClicked(button); 
    }
    
    public boolean mouseDown(int button) 
    { 
        return EmulatedCore.getWindow().mouseDown(button); 
    }
    
    public boolean keyTyped(int key) 
    {
        return EmulatedCore.getWindow().keyTyped(key); 
    }
    
    public boolean keyDown(int key) 
    { 
        return EmulatedCore.getWindow().keyDown(key); 
    }
}
