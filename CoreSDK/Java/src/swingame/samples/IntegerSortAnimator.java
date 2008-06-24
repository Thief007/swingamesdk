/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.samples;

import java.awt.Color;
import swingame.Core;
import swingame.Graphics;
import swingame.Input;
import swingame.Keys;

/**
 *
 * @author acain
 */
public class IntegerSortAnimator 
{
    private final int _max;
    private int[] _data;
    
    public IntegerSortAnimator(int count, int max)
    {
	    _max = max;
        _data = new int[count];
	    Randomize();
    }

    public void Randomize() 
    {
        for(int i = 0; i < _data.length; i++)
        {
            _data[i] = (int)(Math.random() * _max);
        }
    }
    
    public void ShowData()
    {
        Graphics.clearScreen();

        int w = (int)(Core.screenWidth() / _data.length);
        float scaleHeight = Core.screenHeight() / (float)_max;

        for(int i = 0; i < _data.length; i++)
        {         
                float x = w * i;
                float y = (_max - _data[i]) * scaleHeight;
                int h = (int)Math.ceil(_data[i] * scaleHeight);

                Graphics.fillRectangle(Color.RED, x, y, w, h);
        }

        Core.refreshScreen(60);
    }
    
    public void Sort()
    {
        for(int i = _data.length - 1; i >= 0; i--)
        {
            for(int j = 0; j < i; j++)
            {
                Core.processEvents();
                if(Core.windowCloseRequested()) return;
                if(Input.wasKeyTyped(Keys.VK_ESCAPE)) return;
                
                if(_data[j] > _data[j+1])
                {
                    int temp = _data[j];
                    _data[j] = _data[j+1];
                    _data[j+1] = temp;
                    ShowData();
                }
            }
        }
    }

}
