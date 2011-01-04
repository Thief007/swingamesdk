package mygame;

import java.awt.Color;
import java.awt.Point;

import swingame.Graphics;
import swingame.Input;
import swingame.MouseButton;
import swingame.KeyCode;
import swingame.Utils;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class GameMain
{
    static
    {        
        try
        {
            System.loadLibrary("JavaSwinGame");
        }
        finally { System.out.println("Loaded SwinGame native library"); }
    }
    
    public static void main(String[] args)
    {
        //Use the native library using: 
        Utils.useNative();
        
        Graphics.openGraphicsWindow("Hello World", 800, 600);
        
        float x = 0.0f;
        int c = 0xc3;
        int c1 = 0x0f;
        int b = 0x03;
        
        // Color c = Color.RED;
        // Color c1 = Color.BLUE;
        
        while (false == Input.windowCloseRequested())
        {
             Input.processEvents();
             Graphics.refreshScreen();
         }
        //     
        //     // if (Input.mouseClicked(MouseButton.LEFT_BUTTON)) 
        //     // {
        //     //     c = c == Color.RED ? Color.WHITE : Color.RED;
        //     // }
        //     // 
        //     // if (Input.keyTyped(KeyCode.VK_SPACE))
        //     // {
        //     //     c1 = c1 == Color.BLUE ? Color.WHITE : Color.BLUE;
        //     // }
        //     // 
        //     
        //     // Graphics.clearScreen(b);
        //     // Graphics.fillRectangle(c, x, 10.0f, 10, 20);
        //     // Graphics.fillEllipse(c1, x, 40.0f, 20, 10);    
        //     // 
        //     // if( false == Input.keyDown(KeyCode.VK_L) )
        //     //     Graphics.drawLine(Color.WHITE, x, 70.0f, x + 10, 75.0f);
        //     
        //     x += 1;
        //     
        //     // Point p = Input.mousePosition();
        //     // Graphics.drawPixel(Color.GREEN, (float)p.getX(), (float)p.getY());
        //     
        //     // Graphics.refreshScreen();
        // }
    }
}
