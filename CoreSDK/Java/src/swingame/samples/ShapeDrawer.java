package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;
import swingame.Audio;
import swingame.Keys;

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
        Core.openGraphicsWindow("Shape Drawer", 800, 600);
        //Open Audio Device
        Audio.openAudio();
        //Load Resources
        //Resources.loadResources();

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

        //Free Resources and Close Audio, to end the program.
        //Resources.freeResources();
        Audio.closeAudio();   
    }
}
