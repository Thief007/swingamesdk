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
    public class Route
    {
        public readonly City Start;
        public Path[] PathsTaken;
        public int Pos;
        
        public Route(int count, City start)
        {
            PathsTaken = new Path[count];
            Pos = 0;
            Start = start;
        }
        
        public Route Clone()
        {
            Route newRoute = new Route(Pos, Start);
            for(int i = 0; i < Pos; i++)
            {
                newRoute.PathsTaken[i] = PathsTaken[i];
            }
            newRoute.Pos = Pos;
            return newRoute;
        }
        
        public void Clear()
        {
            /*
            for(int i = 0; i < PathsTaken.Length; i++)
            {
                PathsTaken[i] := null;
            }
            */
            Pos = 0;
        }
        
        public int Length
        {
            get
            {
                return Pos;
            }
        }
        
        public void Add(Path p)
        {
            PathsTaken[Pos] = p;
            Pos++;
        }
        
        public double Distance
        {
            get
            {
                double result = 0;
                for(int i = 0; i < Pos; i++)
                {
                    result += PathsTaken[i].Distance;
                }
                return result;
            }
        }
        
        public void IncreasePheromone()
        {
            double invDist = 1 / Distance;
            
            for(int i = 0; i < Pos; i++)
            {
                PathsTaken[i].IncreasePheromone(invDist);
            }            
        }
        
        public void Draw()
        {
            Draw(Color.Red);
        }
        
        public void Draw(Color clr)
        {
            if (Pos == 0) return;
                        
            float ix, iy, jx, jy;
            
            ix = Start.Location.X;
            iy = Start.Location.Y;
            
            for(int j = 0; j < Length; j++)
            {
                jx = PathsTaken[j].Destination.Location.X;
                jy = PathsTaken[j].Destination.Location.Y;

                Graphics.DrawLine(clr, ix, iy, jx, jy);
                
                ix = jx;
                iy = jy;
            }            
        }
    }
}