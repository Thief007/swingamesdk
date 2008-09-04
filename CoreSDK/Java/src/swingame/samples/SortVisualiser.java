/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.samples;

import swingame.Core;
import swingame.Input;
import swingame.Keys;

/**
 * The SortVisualiser is a program that provides a visual animation
 * of the process involved in sorting data.
 *
 * @author acain
 */
public class SortVisualiser 
{
    public static void main(String[] args)
    {
        //Open a new Graphics Window
        Core.openGraphicsWindow("Sort Visualiser", 800, 600);

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
        
        Core.close();
    }
}
