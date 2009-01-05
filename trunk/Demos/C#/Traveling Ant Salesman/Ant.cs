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
    public class Ant
    {
        public static readonly Random Rand = new Random();
        public static readonly double q = 0.90; //chance to take shortest . strongest path
        
        public readonly Route RouteTaken;
        public readonly City StartAt;
        public readonly int StartIdx;
        public readonly City[] Cities;
        
        public City Current;
        public bool[] Memory;
        
        public Ant(City[] cities, int startIdx)
        {
            StartIdx = startIdx;
            Memory = new bool[cities.Length]; //all initialised to False
            Cities = cities;
            StartAt = Cities[startIdx];
            Memory[startIdx] = true;
            RouteTaken = new Route(cities.Length, StartAt);
            Current = StartAt;
            
            //Console.WriteLine("Ant rand: {0}", Rand.Next());
        }
        
        public void EndTour()
        {
            //Current = StartAt;
            RouteTaken.Clear();
            for(int i = 0; i < Memory.Length; i++)
            {
                Memory[i] = i == StartIdx; //false for all but the start...
            }
        }
        
        public void Step()
        {
            Path s = null;
            double q0 = Rand.NextDouble();
            Path[] paths = Current.Exits;
            City tmp;
            
            if (q0 < q)
            {
                //Console.WriteLine("q0 < q - Taking optimal path");
                int idx = 0;
                double weight, max = -1;
                
                //for each City left to visit
                for(int i = 0; i < paths.Length; i++)
                {
                    tmp = paths[i].Destination;
                    
                    //If has visited city
                    if(Memory[tmp.Index]) continue;
                    
                    weight = paths[i].Weight;
                    
                    if(max == -1 || weight > max)
                    {
                        max = weight;
                        s = paths[i];
                    }
                }
            }
            else
            {
                //Console.WriteLine("S - Taking random path");
                                
                //Return S - using probability function                
                double sum = 0;
                
                //for each City left to visit
                for(int i = 0; i < paths.Length; i++)
                {
                    tmp = paths[i].Destination;

                    //If has visited
                    if(Memory[tmp.Index]) continue;
                    
                    sum += paths[i].Weight;
                }
                
                //Console.WriteLine("Sum = {0}", sum);
                
                double take = Rand.NextDouble() * sum;
                sum = 0;

                //Console.WriteLine("Taking {0}", take);

                //for each City left to visit
                for(int i = 0; i < paths.Length; i++)
                {
                    tmp = paths[i].Destination;
                    
                    //If has visited
                    if(Memory[tmp.Index]) continue;
                    
                    sum += paths[i].Weight;
                    if (sum > take)
                    {
                        s = paths[i];
                        break;
                    }
                }                
            }
            
            if (s == null)
            {
                if (RouteTaken.Length != Cities.Length - 1) throw new Exception("None found??");
                
                //Return to first...
                for(int i = 0; i < paths.Length; i++)
                {
                    if (paths[i].Destination == StartAt)
                    {
                        s = paths[i];
                        break;
                    }
                }
            }
            
            //Console.WriteLine("s found...");
            Memory[s.Destination.Index] = true;
            //Console.WriteLine("s remembered");
            
            s.EvaporatePheromone();
            
            RouteTaken.Add(s);
            Current = s.Destination;
        }
        
        public void Draw()
        {
            RouteTaken.Draw();
            Graphics.DrawCircle(Color.Red, Current.Location.X, Current.Location.Y, 3);
        }
    }
}