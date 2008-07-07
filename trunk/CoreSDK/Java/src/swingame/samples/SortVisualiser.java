/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.samples;

import swingame.Audio;
import swingame.Core;
import swingame.Input;
import swingame.Keys;

/**
 *
 * @author acain
 */
public class SortVisualiser 
{
    public static void main(String[] args)
    {
        runGame();
    }
    
    public static void runGame()
    {
        //Open a new Graphics Window
        Core.openGraphicsWindow("Sort Visualiser", 800, 600);

        //Open Audio Device
        Audio.openAudio();

        //Load Resources
        //Resources.LoadResources();

        IntegerSortAnimator2 sa = new IntegerSortAnimator2(80, 100);            

        //Game loop
        sa.sort();
        do
        {
            Core.processEvents();

            if(Input.wasKeyTyped(Keys.VK_R)) sa.randomize();
            if(Input.wasKeyTyped(Keys.VK_S)) sa.sort();
            
            sa.showData();
            
            Core.refreshScreen(60);
        } while (false == Core.windowCloseRequested());

        //Resources.FreeResources()
        Audio.closeAudio();
    }
}
