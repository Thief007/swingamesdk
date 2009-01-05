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
    public class Colony
    {
        public int Following = 0;
        public int StepCount = 0;
        public readonly Ant[] Ants;
        public readonly int MaxSteps;
        public City[] Cities;
        public Route LastBest, OverallBest;
        
        public Colony(int count, City[] cities)
        {
            Ants = new Ant[count];
            Cities = cities;
            MaxSteps = cities.Length;
            
            for(int i = 0; i < count; i++)
            {
                Ants[i] = new Ant(cities, Ant.Rand.Next(cities.Length));
            }
        }
        
        public void Draw()
        {
            if(OverallBest != null)
            {
                OverallBest.Draw(Color.Blue);
                Text.DrawText("" + OverallBest.Distance, Color.White, Resources.GameFont("Courier"), 0, 0);
            }
            //if(LastBest != null) LastBest.Draw(Color.Green);
            Ants[Following].Draw();
        }
        
        public void Step()
        {
            if(StepCount == MaxSteps)
            {
                PerformEndOfTour();
                StepCount = 0;
            }
            else
            {
                StepCount++;
                //Console.WriteLine("Step = {0}", StepCount);                
                
                for(int i = 0; i < Ants.Length; i++)
                {
                    Ants[i].Step();
                }
            }
        }
        
        private void PerformEndOfTour()
        {
            //Console.WriteLine("Ending Tour");
            
            //Reduce Pheromone
            for(int i = 0; i < Cities.Length; i++)
            {
                Cities[i].ReducePheromone();
            }
            
            double tmp, dist = Ants[0].RouteTaken.Distance;
            Ant best = Ants[0];
            
            //Find Best...
            for(int i = 1; i < Ants.Length; i++)
            {
                tmp = Ants[i].RouteTaken.Distance;
                if (tmp < dist)
                {
                    best = Ants[i];
                    dist = tmp;
                }
            }
            
            //Increase its Pheromone
            best.RouteTaken.IncreasePheromone();
            
            //Remember it...
            LastBest = best.RouteTaken.Clone();
            
            if(OverallBest == null || LastBest.Distance < OverallBest.Distance) OverallBest = LastBest;
            
            //Clear routes
            for(int i = 0; i < Ants.Length; i++)
            {
                Ants[i].EndTour();
            }
        }
    }
}