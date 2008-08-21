/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;
import swingame.MouseButton;

/**
 *
 * @author acain
 */
public class Mandelbrot 
{
    public static final int MAX_ITERATIONS = 1000;
    public static final int SCREEN_WIDTH = 320;
    public static final int SCREEN_HEIGHT = 240;
    
    
    public static Color mandelbrotColor(float x0, float y0)
    {
        float x, y, xtemp;
        int iteration;

        x = x0;  //the x co-ordinate of pixel in Mandelbrot set coordinates
        y = y0;  //the y co-ordinate of pixel in Mandelbrot set coordinates

        iteration = 0;

        while ((x*x + y*y <= 2*2)  &&  (iteration < MAX_ITERATIONS))
        {
            xtemp = x*x - y*y + x0;
            y = 2*x*y + y0;

            x = xtemp;

            iteration = iteration + 1;
        }
 
        return mapColor(iteration);
    }
    
    public static Color mapColor(int iteration)
    {
        if (iteration == MAX_ITERATIONS)
            return Color.BLACK;
        else
        {
            float hue = (iteration / (float)MAX_ITERATIONS) + (180 / 360.0f);
  
            if (hue > 1) hue = hue - 1;
  
            return Core.getHSBColor(hue, 1.0f, 0.9f);
        }
    }
    
    public static void drawMandelbrot(float setX, float setY, float setWidth, float setHeight)
    {
        Color color;
        float scaleWidth, scaleHeight;
        
        scaleWidth = setWidth / SCREEN_WIDTH;
        scaleHeight = setHeight / SCREEN_HEIGHT;
        
        for (int x = 0; x < SCREEN_WIDTH; x++)
        {
            for (int y = 0; y < SCREEN_HEIGHT; y++)
            {   
                color = mandelbrotColor(setX + x * scaleWidth, setY + y * scaleHeight);
                Graphics.drawPixel(color, x, y);
            }
        }
    }
    
    public static void main(String[] args)
    {
        float setX, setY, setWidth, setHeight;
        
        setX = -2.5f;
        setY = -1.5f;
        setWidth = 4.0f;
        setHeight = 3.0f;

        //Open a new Graphics Window
        Core.openGraphicsWindow("Mandelbrot", SCREEN_WIDTH, SCREEN_HEIGHT);


        drawMandelbrot(setX, setY, setWidth, setHeight);

        //Game loop
        do
        {
            Core.processEvents();

            if(Input.mouseWasClicked(MouseButton.LEFT_BUTTON))
            {
                Point mouse;
		mouse = Input.getMousePosition();

		//TODO: position setX based on mouse.X
		//TODO: position setY based on mouse.Y
		setX = setX + (float)mouse.getX() / SCREEN_WIDTH * setWidth - setWidth / 4.0f;
                setY = setY + (float)mouse.getY() / SCREEN_HEIGHT * setHeight - setHeight / 4.0f;
                setWidth = setWidth / 2.0f;
		setHeight = setHeight / 2.0f;

		drawMandelbrot(setX, setY, setWidth, setHeight);
            }

            Core.refreshScreen(60);
        } while (false == Core.windowCloseRequested());

        Core.close();
    }
}
