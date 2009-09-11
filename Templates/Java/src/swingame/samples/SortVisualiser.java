/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.samples;

import swingame.Core;
import swingame.Input;
import swingame.KeyCode;

/**
 * The SortVisualiser is a program that provides a visual animation
 * of the process involved in sorting data.
 *
 * @author acain
 */
public class SortVisualiser 
{
    /**
    * The entry point for the program.
    *
    * @param args   Command line arguments - not used
    */
    public static void main(String[] args)
    {
        //Open a new Graphics Window
        Core.openGraphicsWindow("Sort Visualiser", 800, 600);

        IntegerSortAnimator2 sa = new IntegerSortAnimator2(80, 100);            

        //Game loop
        sa.sort();      // Sort when it starts
        do
        {
            Core.processEvents();

            if(Input.keyTyped(KeyCode.VK_R)) sa.randomize();
            if(Input.keyTyped(KeyCode.VK_S)) sa.sort();
            
            sa.showData();
            
            Core.refreshScreen(60);
        } while (false == Core.windowCloseRequested());
        
        Core.close();
    }
}