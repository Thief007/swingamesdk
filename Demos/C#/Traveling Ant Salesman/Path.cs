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
    public class Path
    {
        public static readonly double a = 0.1; //take a% of new pheromone
        public static readonly double a1 = 0.1; //Evaporate %
        public static readonly double B = 2.0   ; //weight for closeness vs. pheromone
        public static double T = 1.0;
        
        public readonly double Distance;
        public readonly City Destination;

        public double Pheromone = 0.1;
        
        public Path(City dest, double dist)
        {
            Destination = dest;
            Distance = dist / 1131.370849898476; // Longest path
        }
        
        public void ReducePheromone()
        {
            Pheromone *= (1 - a);
        }
        
        public void EvaporatePheromone()
        {
            Pheromone = (1 - a1) * Pheromone + a1 * T;
        }
        
        public void IncreasePheromone(double diff)
        {
            Pheromone += a * diff;
        }
        
        public double InverseDistance
        {
            get
            {
                return 1 / Distance;
            }
        }
        
        public double Weight
        {
            get
            {
                return Pheromone * Math.Pow(InverseDistance, B);
            }
        }
    }
}