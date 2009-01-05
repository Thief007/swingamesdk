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
    public class City
    {
        private static readonly Random Rand = new Random();
        
        public readonly Point2D Location;
        public readonly int Index;
                
        public Path[] Exits;
        
        public City(int count, int idx)
        {
            Exits = new Path[count - 1];
            Location = new Point2D(Rand.Next(800), Rand.Next(800));
            Console.WriteLine("City {0} at {1}:{2}", idx, Location.X, Location.Y);
            Index = idx;
        }
        
        public void AddDestinations(City[] cities)
        {
            int offset = 0;
            for(int i = 0; i < cities.Length; i++)
            {
                if(cities[i] == this) offset = 1; //skip self
                else Exits[i - offset] = new Path(cities[i], Shapes.DistanceBetween(Location, cities[i].Location));
            }
        }
        
        public void ReducePheromone()
        {
            for(int i = 0; i < Exits.Length; i++)
            {
                Exits[i].ReducePheromone();
            }
        }
    }
}