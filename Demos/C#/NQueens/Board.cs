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
using Queen = GameProject.Queen;

using GameResources;

namespace GameProject
{
    public class Board : IEnumerable<Queen>
    {
        public readonly Random Rand = new Random();
        public readonly int NumQueens;
        public readonly Queen[] Queens;
        public readonly Color QueenColor;
        public readonly Diagonal[][] Diagonals;
        public readonly DateTime Start;
        
        public int CurrentConflicts;
                
        public Board(int n)
        {
            Start = DateTime.Now;
            QueenColor = Core.GetColor(255, 255, 255, 120);
            Queens = new Queen[n];
            NumQueens = n;
            jIdx = NumQueens;
            
            rowsChecked = new bool[NumQueens];
            
            Diagonals = new Diagonal[2][];
            
            for(int i = 0; i < 2; i++)
            {
                Diagonals[i] = new Diagonal[2 * NumQueens - 1];
                
                for(int j = 0; j < 2 * NumQueens - 1; j++)
                {
                    Diagonals[i][j] = new Diagonal();
                    Diagonals[i][j].Queens = 0;
                }
            }
            
            int[] rows = new int[NumQueens];
            
            for(int i = 0; i < NumQueens; i++)
            {
                rows[i] = i;
            }
            
            for(int i = 0; i < NumQueens; i++)
            {
                int j = Rand.Next(NumQueens);
                
                int temp = rows[j];
                rows[j] = rows[i];
                rows[i] = temp;
            }
            
            for(int i = 0; i < NumQueens; i++)
            {
                Queens[i] = new Queen(this, i, rows[i]);
                
                
                if(i % 100 == 0)
                {
                    Graphics.ClearScreen();
                    
                    Text.DrawText("" + (int)(i / (float)NumQueens * 100) + "% Loaded", Color.White, Resources.GameFont("Courier"), 10, 400);
                    
                    Core.RefreshScreen();
                    Core.ProcessEvents();
                    
                    if(Core.WindowCloseRequested()) return;
                }
            }
            
            CurrentConflicts = TotalConflicts;
        }
        
        public Diagonal[] PositiveDiagonal
        {
            get { return Diagonals[0]; }
        }
        
        public Diagonal[] NegativeDiagonal
        {
            get { return Diagonals[1]; }
        }
        
        public IEnumerator<Queen> GetEnumerator()
        {
            foreach(GameProject.Queen q in Queens) yield return q;
        }
        
        IEnumerator IEnumerable.GetEnumerator()
        {
            foreach(GameProject.Queen q in Queens) yield return q;
        }
        
        public Queen this[int idx]
        {
            get
            {
                return Queens[idx];
            }
        }
        
        public void Draw()
        {
            Graphics.ClearScreen();
            
            foreach(Queen q in this)
            {
                int conflicts = q.Conflicts;
                
                int x, y;
                
                x = (int)((q.Column / (float)NumQueens) * 800);
                y = (int)((q.Row / (float)NumQueens) * 800);
                                
                switch(conflicts)
                {
                    case 0: Graphics.DrawPixel(QueenColor, x, y); break;
                    case 1: Graphics.DrawPixel(Color.Yellow, x, y); break;
                    case 2: Graphics.DrawPixel(Color.Green, x, y); break;
                    case 3: Graphics.DrawPixel(Color.Purple, x, y); break;                    
                    default: Graphics.DrawPixel(Color.Red, x, y); break;
                }
            }
        }
        
        private int AtQueen = -1; //will do ++ before first check
        private int jIdx;
        private int steps = 0;
        
        private Queen NextQueen()
        {   
            jIdx++;
            
            if (jIdx % 1000 == 0) Console.Write(".");
            
            if(jIdx >= NumQueens)
            {
                steps++;
                AtQueen = (AtQueen + 1) % (NumQueens - 1);
                jIdx = AtQueen + 1;
                Console.WriteLine("\nStep {0}", steps);
            }

            return Queens[AtQueen];
        }
        
        private Queen NextConflictingQueen()
        {   
            if(CurrentConflicts <= 0) return null;
            
            AtQueen = (AtQueen + 1) % NumQueens;
            
            //Console.WriteLine("Conflicts = {0} == {1}", CurrentConflicts, TotalConflicts);
            while(Queens[AtQueen].Conflicts == 0) 
            {
                AtQueen = (AtQueen + 1) % NumQueens;
            }
            steps++;
            if(steps % 100 == 0)
                Console.Write(".");
            return Queens[AtQueen];
        }
        
        private Queen NextQueen2()
        {   
            if(CurrentConflicts <= 0) return null;
            
            if (AtQueen > -1) return Queens[AtQueen];
            
            //Console.WriteLine("Conflicts = {0} == {1}", CurrentConflicts, TotalConflicts);
            ResetRowsChecked();
            
            
            FindMostConflicts();
            
            AtQueen = 0;
            while(Queens[AtQueen].Conflicts == 0) 
            {
                AtQueen = (AtQueen + 1);
            }

            steps++;
            Console.Write(".");

            return Queens[AtQueen];
        }
        
        private bool[] rowsChecked;
        
        private void ResetRowsChecked()
        {
            for(int r = 0; r < NumQueens; r++) rowsChecked[r] = false;
        }
        
        public void StepToSolution()
        {
            if(CurrentConflicts == 0) return;

            Queen q = NextQueen2();
            
            int rowToSwap = q.BestRowToMoveTo(rowsChecked, out AtQueen);
            int otherIdx = -1;
            int startConflicts = q.Conflicts;
            
/*            for(int r = 0; r < NumQueens; r++)
            {
                if (r == AtQueen) continue;
                if (Queens[r].Row == rowToSwap)
                {
                    otherIdx = r;
                    break;
                }
            }
 */
            
            q.RemoveQueen();
            q.PutQueenBackAt(rowToSwap);
            rowsChecked[rowToSwap] = true;
            
            //AtQueen = otherIdx;
            
            CurrentConflicts -= (startConflicts - q.Conflicts) * 2;

            if(CurrentConflicts == 0)
            {
                Console.WriteLine("At solution in {0} seconds", DateTime.Now.Subtract(Start).TotalSeconds);
                
            }
        }
        
        public void StepToSolutionBestSwap()
        {
            if(CurrentConflicts == 0) return;

            Queen qi = NextConflictingQueen();
            qi.DoBestSwap();            
            
            if(CurrentConflicts == 0)
            {
                Console.WriteLine("At solution in {0} seconds", DateTime.Now.Subtract(Start).TotalSeconds);
                
            }
        }
        
        public void StepToSolutionGradient()
        {
            if(CurrentConflicts == 0) return;
            
            int start, current;
            
            Queen qi = NextQueen();
            Queen qj = Queens[jIdx];
            
            if(qi.Conflicts > 0 || qj.Conflicts > 0)
            {
                start = qi.Conflicts + qj.Conflicts;
                
                qi.SwapWith(qj);
                current = qi.Conflicts + qj.Conflicts;
                
                if(start <= current)
                {
                    //Not a good swap
                    qi.SwapWith(qj); //reverse swap
                }
                else
                {
                    //Get new conflicts
                    CurrentConflicts -= (start - current) * 2;
                    //Console.WriteLine("Conflicts = {0} == {1}", CurrentConflicts, TotalConflicts);
                    //Console.WriteLine("Start = {0} Current = {1}", start, current);
                }
            }
        }
        
        public int TotalConflicts
        {
            get
            {
                int result = 0;
                foreach(Queen q in this) result += q.Conflicts;
                return result;
            }
        }
        
        public int QueenAtRow(int r)
        {
            for(int i = 0; i < NumQueens; i++) if (Queens[i].Row == r) return i;
            return -1;
        }
          
        private int FindMostConflicts()
        {
            int max = 0;
            Queen result = Queens[0];
            int ret = -1;
            
            for(int i = 1; i < NumQueens; i++)
            {
                if (Queens[i].Conflicts > max)
                {
                    ret = i;
                    max = Queens[i].Conflicts;
                }
            }
            
            return ret;
        }
    }
}