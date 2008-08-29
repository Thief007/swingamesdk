package mygame;

import java.awt.Color;
import java.awt.Point;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;

import swingame.MouseButton;
import swingame.Keys;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class Sample
{
    public static void main(String[] args)
    {
        //Use the native library using: 
        //Core.useNative();
        
        Core.openGraphicsWindow("Hello World", 800, 600);
        
        float x = 0.0f;
        Color c = Color.RED;
        Color c1 = Color.BLUE;
        
        while (false == Core.windowCloseRequested())
        {
            Core.processEvents();
            
            if (Input.mouseWasClicked(MouseButton.LEFT_BUTTON)) 
            {
                c = c == Color.RED ? Color.WHITE : Color.RED;
            }
            
            if (Input.wasKeyTyped(Keys.VK_SPACE))
            {
                c1 = c1 == Color.BLUE ? Color.WHITE : Color.BLUE;
            }
            
            Graphics.clearScreen(Color.BLACK);
            Graphics.fillRectangle(c, x, 10.0f, 10, 20);
            Graphics.fillEllipse(c1, x, 40.0f, 20, 10);    
            
            if( false == Input.isKeyPressed(Keys.VK_L) )
                Graphics.drawLine(Color.WHITE, x, 70.0f, x + 10, 75.0f);
            
            x += 1;
            
            Point p = Input.getMousePosition();
            Graphics.drawPixel(Color.GREEN, (float)p.getX(), (float)p.getY());
            
            Core.refreshScreen();
        }
    }
}
