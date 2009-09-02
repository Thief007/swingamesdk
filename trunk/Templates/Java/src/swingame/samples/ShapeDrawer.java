package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;
import swingame.KeyCode;

/**
 * The ShapeDrawer is the main entrance point for the drawing sample
 * program. This is an illustration of inheritance and object interaction.
 *
 * @author  Andrew Cain
 */
public final class ShapeDrawer 
{
    public static void main(String args[])
    {
        //Open a new Graphics Window
        /*Core.openGraphicsWindow("Shape Drawer", 800, 600);

        Drawing myDrawing = new Drawing();

        //Create and add a Shape
        Shape s;
        Point p;

        s = new Rectangle();
        s.setColor(Color.BLUE);
        s.setWidth(30);
        s.setHeight(50);
        p = new Point();
        p.setLocation(75, 25);
        s.setPosition(p);

        myDrawing.addShape(s);

        //Test selecting a Shape
        p = new Point();
        p.setLocation(100, 50);
        myDrawing.selectShape(p);

        if ( s != myDrawing.getSelectedShape() ) System.out.println("Select Shape not working!");

        //Game Loop
        do
        {
            Graphics.clearScreen();
            myDrawing.draw();

            Core.refreshScreen();
            Core.processEvents();
        } while (!Core.windowCloseRequested());*/
        // Core.useNative();
        runGame();
    }

    public static void runGame()
    {
        //Open a new Graphics Window
        Core.openGraphicsWindow("Shape Drawer", 800, 600);

        DrawingController dc = new DrawingController();

        //Game Loop
        do
        {
            Graphics.clearScreen();
            dc.draw();
            Core.refreshScreen(60);

            Core.processEvents();
            dc.handleInput();
        } while (!Core.windowCloseRequested());
    }
}
