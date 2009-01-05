using System;
using System.Text;
using System.Drawing;
using System.Collections;
using System.Collections.Generic;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;

namespace GameProject
{
    public class RandomMap
    {
        public Color LineBase;
        public City[] Cities;
        
        public RandomMap(int count)
        {
            LineBase = Color.FromArgb(10, 255, 255, 255);
            
            Cities = new City[count];
            for(int i = 0; i < count; i++)
            {
                Cities[i] = new City(count, i);
            }
            
            for(int i = 0; i < count; i++)
            {
                Cities[i].AddDestinations(Cities);
            }
            
            CalculateT();
        }
        
        private void CalculateT()
        {
            List<City> visited = new List<City>();
            City current = Cities[0];
            visited.Add(Cities[0]);
            double min, currLen, totLen = 0;
            Path mP;
            
            for(int i = 1; i < Cities.Length; i++)
            {
                min = -1;
                mP = null;
                
                for(int p = 0; p < current.Exits.Length; p++)
                {
                    //Skip ones I have visited
                    if (visited.Contains(current.Exits[p].Destination)) continue;
                    
                    currLen = current.Exits[p].Distance;
                    if (min == -1 || currLen < min)
                    {
                        min = currLen;
                        mP = current.Exits[p];
                    }
                }
                
                if (mP == null) throw new Exception("No mP?");
                
                current = mP.Destination;
                totLen += min;
                visited.Add(current);
            }

            for(int p = 0; p < current.Exits.Length; p++)
            {
                if (current.Exits[p].Destination == Cities[0])
                {
                    totLen += current.Exits[p].Distance;
                    break;
                }
            }
            
            Path.T = 1 / (Cities.Length * totLen);
            Console.WriteLine("T is {0} -  from Lnn {1}", Path.T, totLen);
            
        }
        
        public void Draw()
        {
            float ix, iy, jx, jy;
            for(int i = 0; i < Cities.Length; i++)
            {
                ix = Cities[i].Location.X;
                iy = Cities[i].Location.Y;
                
                for(int j = i + 1; j < Cities.Length; j++)
                {
                    //if(j == i) continue;
                    
                    jx = Cities[j].Location.X;
                    jy = Cities[j].Location.Y;

                    Graphics.DrawLine(LineBase, ix, iy, jx, jy);
                }
                
                Graphics.DrawCircle(Color.White, ix, iy, 2);
            }
        }
    }
}