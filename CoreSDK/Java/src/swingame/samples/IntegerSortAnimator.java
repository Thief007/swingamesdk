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
    private final int _Max;
    private int[] _Data;
    
    public IntegerSortAnimator(int count, int max)
    {
	_Max = max;
        _Data = new int[count];
	Randomize();
    }

    public void Randomize() 
    {
        for(int i = 0; i < _Data.length; i++)
        {
            _Data[i] = (int)(Math.random() * _Max);
        }
    }
    
    public void ShowData()
    {
        Graphics.clearScreen();

        int w = (int)(Core.screenWidth() / _Data.length);
        float scaleHeight = Core.screenHeight() / (float)_Max;

        for(int i = 0; i < _Data.length; i++)
        {         
                float x = w * i;
                float y = (_Max - _Data[i]) * scaleHeight;
                int h = (int)Math.ceil(_Data[i] * scaleHeight);

                Graphics.fillRectangle(Color.RED, x, y, w, h);
        }

        Core.refreshScreen(60);
    }
    
    public void Sort()
    {
        for(int i = _Data.length - 1; i >= 0; i--)
        {
            for(int j = 0; j < i; j++)
            {
                Core.processEvents();
                if(Core.windowCloseRequested()) return;
                if(Input.wasKeyTyped(Keys.VK_ESCAPE)) return;
                
                if(_Data[j] > _Data[j+1])
                {
                    int temp = _Data[j];
                    _Data[j] = _Data[j+1];
                    _Data[j+1] = temp;
                    ShowData();
                }
            }
        }
    }

}
