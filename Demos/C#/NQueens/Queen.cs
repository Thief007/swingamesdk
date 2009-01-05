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
    public class Queen
    {
        //public int Conflicts;
        public int Row;
        public readonly int Column;
        public readonly Board OnBoard;
        
        public Queen(Board b, int col, int row)
        {
            OnBoard = b;
            Column = col; 
            Row = row;
            UpdateConflicts();
            
            //Console.Write("Creating queen {0}", col);
            
            //PlaceRandom();
            
            //Console.WriteLine(" at row {0}", Row);
        }
        
        public Diagonal PositiveDiagonal
        {
            get
            {
                return OnBoard.Diagonals[0][OnBoard.NumQueens + Column - Row - 1];
            }
        }
        
        public Diagonal NegativeDiagonal
        {
            get
            {
                return OnBoard.Diagonals[1][Column + Row];    
            }
        }
        
        public int Conflicts
        {
            get
            {
                return PositiveDiagonal.Conflicts + NegativeDiagonal.Conflicts;
            }
        }
        
        public void UpdateConflicts()
        {
            PositiveDiagonal.Queens++;
            NegativeDiagonal.Queens++;
            //Conflicts = ConflictsAt(Row);
        }
        
        public void RemoveQueen()
        {
            /*foreach(Queen q in OnBoard)
            {
                if (q == this) continue;
                //if this queen conflicts with queen q - reduce conflicts
                if ( Math.Abs(q.Row - Row) == Math.Abs(q.Column - Column) )
                {
                    q.Conflicts--;
                    Conflicts--;
                    if(Conflicts == 0) return;
                }
            }*/
            
            PositiveDiagonal.Queens--;
            NegativeDiagonal.Queens--;           
        }

        public void PutQueenBackAt(int row)
        {
            Row = row;
            /*Conflicts = 0;
            
            foreach(Queen q in OnBoard)
            {
                if (q == this) continue;
                
                //if this queen conflicts with queen q - increase conflicts of both
                if ( Math.Abs(q.Row - Row) == Math.Abs(q.Column - Column) )
                {
                    q.Conflicts++;
                    Conflicts++;
                } 
            }*/
            
            PositiveDiagonal.Queens++;
            NegativeDiagonal.Queens++;                   
        }
        
        public int ConflictsAt(int row)
        {
            int result;
            
            result = OnBoard.Diagonals[0][OnBoard.NumQueens + Column - row - 1].Conflicts;
            result += OnBoard.Diagonals[1][Column + row].Conflicts; 
            
            return result;
        }

        public int BestRowToMoveTo(bool[] rowsToExclude, out int otherQueen)
        {
            int min = OnBoard.NumQueens;
            int row = -1;
            int temp;
            int tempQueen;
            
            otherQueen = -1;
            
            for(int r = 0; r < OnBoard.NumQueens; r++)
            {
                if (r == Row) continue;
                if (rowsToExclude[r]) continue;
                
                temp = ConflictsAt(r);
                tempQueen = OnBoard.QueenAtRow(r);
                
                if(tempQueen >= 0 && OnBoard.Queens[tempQueen].Conflicts == 0) temp++;
                
                if(temp < min)
                {
                    min = temp;
                    row = r;
                    otherQueen = tempQueen;
                }
            }
            
            return row;
        }
        
        public void SwapWith(Queen other)
        {
            RemoveQueen();
            other.RemoveQueen();
            
            int otherRow = other.Row;
            
            other.PutQueenBackAt(Row);
            PutQueenBackAt(otherRow);
        }
        
        public void MaxFromExchange(int r, int start, ref int max, ref Queen best)
        {
            Queen other = OnBoard[r];
            int otherStart = other.Conflicts;
            
            SwapWith(other);
            
            int now = Conflicts;
            int otherNow = other.Conflicts;
            
            if ((start - now) + (otherStart - otherNow) > max)
            {
                best = other;
                max = (start - now) + (otherStart - otherNow);
            }
            
            SwapWith(other);
        }
        
        
        public void DoBestSwap()
        {
            if (Conflicts == 0)
            {
                //Console.WriteLine("Already at solution for this queen...");
                return;
            }
            
            Queen best = null;
            int myRow = Row; //, otherRow;
            
            int max = 0;
            int start; //, now, otherStart, otherNow;
            
            start = Conflicts;
            
            for(int r = Row + 1; r < OnBoard.NumQueens; r++)
            {
                //if((Row + r) % OnBoard.NumQueens == Row) continue;
                MaxFromExchange(r, start, ref max, ref best);
            }
            
            if(max == 0)
            {
                for(int r = 0; r < Row; r++)
                {
                    //if((Row + r) % OnBoard.NumQueens == Row) continue;
                    MaxFromExchange(r, start, ref max, ref best);
                }                
            }
            
            if(max > 0)
            {
                SwapWith(best);
                OnBoard.CurrentConflicts -= max * 2;
            }
        }

        public void PlaceRandom()
        {
            bool found;
            int r = OnBoard.Rand.Next(OnBoard.NumQueens);
            
            do
            {                            
                found = true;
                foreach(Queen q in OnBoard)
                {
                    if(q == this) continue;
                    if(q == null) break; //during creation
                    
                    if (q.Row == r)
                    {
                        found = false;
                        r = (r + 1) % OnBoard.NumQueens;
                        break;
                    }
                }
            } while(false == found);
            
            Row = r;
            UpdateConflicts();
        }
    }
}
