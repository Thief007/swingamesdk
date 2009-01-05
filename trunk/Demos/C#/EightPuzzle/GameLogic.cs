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
    public static class GameLogic
    {
        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("Game", 800, 600);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
            Resources.LoadResources();
            
            Puzzle p = new Puzzle(new Color[]{
                    Core.GetColor(255, 0, 0),
                    Core.GetColor(0, 255, 0),
                    Core.GetColor(0, 0, 255),
                    Core.GetColor(255, 255, 0),
                    Core.GetColor(255, 0, 255),
                    Core.GetColor(0, 255, 255),
                    Core.GetColor(120, 120, 120),
                    Color.Black,
                    Core.GetColor(255, 255, 255)
                }, new Point2D[]{
                    new Point2D(0, 1), // 0
                    new Point2D(2, 0), // 1
                    new Point2D(1, 1), // 2
                    new Point2D(1, 0), // 3
                    new Point2D(2, 1), // 4
                    new Point2D(1, 2), // 5
                    new Point2D(0, 2), // 6
                    new Point2D(0, 0), // 7
                    new Point2D(2, 2), // 8
                });

            Piece moving = null;
            int i = 0, atStep = 0;
            Queue<Piece> steps = null;
                        

            //Game Loop
            do
            {
                if(p.StepTowardSolution())
                {
                    //Has a solution
                    if(atStep % 100 == 0 && (steps == null || steps.Count == 0))
                    {
                        if(moving != null)
                        {
                            moving.SwapWith(p.EmptyPiece);
                            moving.Offset = new Point2D(0, 0);
                        }
                        moving = null;
                        atStep = 0;
                        steps = new Queue<Piece>(p.Best.Moves);
                        p.Reset();
                        
                        Console.WriteLine("Solution = {1} -> {0}", p.Best.State, p.ToKey());
                        foreach(Piece pi in steps)
                        {
                            Console.WriteLine("Move - {0}", pi.Key);
                        }
                    }
                    
                    if(atStep % 100 == 0) 
                    {
                        if(moving != null)
                        {
                            moving.SwapWith(p.EmptyPiece);
                            moving.Offset = new Point2D(0, 0);
                        }
                        
                        if(steps.Count > 0) moving = steps.Dequeue();
                        atStep = 1;
                    }
                    else
                    {
                        int dX = 0, dY = 0;
                        
                        if(p.EmptyPiece.CurrentLocation.X != moving.CurrentLocation.X)
                            dX = (p.EmptyPiece.CurrentLocation.X < moving.CurrentLocation.X ? -1: 1);
                        else
                            dY = (p.EmptyPiece.CurrentLocation.Y < moving.CurrentLocation.Y ? -1: 1);
                            
                        atStep++;
                        moving.Offset.X += dX;
                        moving.Offset.Y += dY;
                    }
                    
                    Graphics.ClearScreen();

    		        Text.DrawFramerate(0, 0, Resources.GameFont("Courier"));
                    p.Draw();
                    Text.DrawText("Solved!", Color.White, Resources.GameFont("ArialLarge"), 50, 500);

                    Core.RefreshScreen(60);
                    Core.ProcessEvents();                    
                }
                else
                {
                    //Refreshes the Screen and Processes Input Events
                    if(i++ % 10 == 0) 
                    {
                        //Clears the Screen to Black
                        Graphics.ClearScreen();

        		        Text.DrawFramerate(0, 0, Resources.GameFont("Courier"));
                        p.Draw();
                        Text.DrawText("Thinking...", Color.White, Resources.GameFont("ArialLarge"), 50, 500);

                        Core.RefreshScreen();
                        Core.ProcessEvents();
        		    }                    
                }
            } while (!Core.WindowCloseRequested());

            //Free Resources and Close Audio, to end the program.
            Resources.FreeResources();
            Audio.CloseAudio();
        }
    }
    
    public class Piece
    {
        public Point2D CurrentLocation;
        public Point2D StartLocation;
        public Point2D Destination;
        public Color Value;
        public Puzzle OnPuzzle;
        public char Key;
        
        public Point2D Offset = new Point2D(0, 0);
        
        public void Draw()
        {
            if(IsEmpty) return;
            
            Graphics.FillRectangle(Value, 
                50 + 100 * CurrentLocation.X + Offset.X,
                100 + 100 * CurrentLocation.Y + Offset.Y,
                99, 99);

            Graphics.FillRectangle(Value, 
                450 + 100 * Destination.X,
                100 + 100 * Destination.Y,
                99, 99);
            
            /*if (false == IsEmpty)
            {
                Text.DrawText("H3: " + HValue, Color.Black, Resources.GameFont("Courier"), (int)(50 + 100 * CurrentLocation.X), (int)(100 + 100 * CurrentLocation.Y));
                Text.DrawText("CL: " + CorrectLocation, Color.Black, Resources.GameFont("Courier"), (int)(50 + 100 * CurrentLocation.X), (int)(120 + 100 * CurrentLocation.Y));
            }*/
        }
        
        public int ManhattenDistance
        {
            get { return (int)(Math.Abs(CurrentLocation.X - Destination.X) + Math.Abs(CurrentLocation.Y - Destination.Y)); }
        }
        
        public int HValue
        {
            get
            {
                return ManhattenDistance + 2 * KValue;
            }
        }
        
        public bool CorrectLocation
        {
            get { return CurrentLocation.X == Destination.X && CurrentLocation.Y == Destination.Y; }
        }
        
        public bool IsEmpty
        {
            get { return Value == Color.Black; }
        }
        
        public void SwapWith(Piece p)
        {
            OnPuzzle.Pieces[(int)CurrentLocation.X, (int)CurrentLocation.Y] = p;
                        
            Point2D dest = p.CurrentLocation;
            p.CurrentLocation = CurrentLocation;
            CurrentLocation = dest;
            
            OnPuzzle.Pieces[(int)CurrentLocation.X, (int)CurrentLocation.Y] = this;
        }
        
        public List<Piece> Neighbours
        {
            get
            {
                List<Piece> result = new List<Piece>();
                
                int c = (int)CurrentLocation.X; 
                int r = (int)CurrentLocation.Y;
                
                if (c > 0) result.Add(OnPuzzle.Pieces[c - 1, r]);
                if (r > 0) result.Add(OnPuzzle.Pieces[c, r - 1]);
                if (c < Puzzle.SIDES - 1) result.Add(OnPuzzle.Pieces[c + 1, r]);
                if (r < Puzzle.SIDES - 1) result.Add(OnPuzzle.Pieces[c, r + 1]);
                
                return result;
            }
        }
        
        public int KValue
        {
            get
            {
                if (ManhattenDistance == 1) 
                {
                    if(OnPuzzle.Pieces[(int)Destination.X, (int)Destination.Y].Destination.X == CurrentLocation.X && OnPuzzle.Pieces[(int)Destination.X, (int)Destination.Y].Destination.Y == CurrentLocation.Y) return 1;
                }
                
                return 0;
            }
        }
    }

    public class Move
    {
        public int NewMHD;
        public Piece ToMove;
        
        public Move(int mhd, Piece moved)
        {
            NewMHD = mhd;
            ToMove = moved;
        }
    }
    
    public class Node : IComparable
    {
        //State is index order of Pieces 0..8
        public string State;
        
        //How we got here...
        public List<Piece> Moves;

        //The puzzle the node is a move in
        public Puzzle InPuzzle;
        
        public int HValue;
        public int Depth;

        ///<summary>
        /// Create the root node for the puzzle.
        ///</summary>
        public Node(Puzzle puz)
        {
            InPuzzle = puz;
            Moves = new List<Piece>();
            
            State = InPuzzle.ToKey();
            Depth = 0;
            HValue = InPuzzle.TotalHValue;
        }
        
        public Node(Node copyOf, Piece nextMove)
        {
            InPuzzle = copyOf.InPuzzle;
            Moves = new List<Piece>(copyOf.Moves);
            
            //Initial State
            State = copyOf.State;

            //Determine new state
            SetPuzzle(InPuzzle);
            Moves.Add(nextMove);
            nextMove.SwapWith(InPuzzle.EmptyPiece);            
            
            //State after move...
            State = InPuzzle.ToKey();
            
            //Calculate cost values
            HValue = InPuzzle.TotalHValue;
            Depth = copyOf.Depth + 1;
            
        }
        
        public int CompareTo(Object o)
        {
            Node n = (Node)o;
            
            if(State == n.State) return 0;
            return Cost - n.Cost;
        }
        
        public int Cost
        {
            get { return HValue + Depth; }
        }
        
        public bool IsAtGoal()
        {
            return HValue == 0;
        }
        
        public void SetPuzzle(Puzzle puz)
        {
            int c, r, idx;
            
            for(int i = 0; i < State.Length; i++)
            {
                idx = ((int)State[i]) - 48;
                c = i / Puzzle.SIDES;
                r = i - (c * Puzzle.SIDES);
                
                puz.OriginalPieces[idx].CurrentLocation = new Point2D(c, r);
                puz.Pieces[c, r] = puz.OriginalPieces[idx];
            }
        }
        
        public List<Node> Expand(Dictionary<Node, Node> closed)
        {
            SetPuzzle(InPuzzle);            
            //Console.WriteLine("N: {0}   P: {1}", State, InPuzzle.ToKey());
            
            List<Node> result = new List<Node>();
            List<Piece> neighbours = InPuzzle.EmptyPiece.Neighbours;
            
            foreach(Piece p in neighbours)
            {
                Node tmp = new Node(this, p);
                if(false == closed.ContainsKey(tmp))
                {
                    result.Add(tmp);
                }
            }
                        
            return result;
        }
        
        public static bool operator == (Node n, Node n1)
        {
            return n.State == n1.State;
        }
        
        public static bool operator != (Node n, Node n1)
        {
            return n.State != n1.State;
        }
        
        public override bool Equals(Object other)
        {
            return this == other as Node;
        }
        
        public override int GetHashCode()
        {
            return State.GetHashCode();
        }
    }
    
    public class Puzzle
    {
        public static readonly int SIDES = 3;
        public static readonly int MAX_DEPTH = 20;
        
        public Node Best = null;
        private bool Quit = false;

        public Piece[,] Pieces = new Piece[SIDES,SIDES];
        public Piece[] OriginalPieces = new Piece[SIDES * SIDES];
        public Piece EmptyPiece;
        
        private Dictionary<Node, Node> Closed = new Dictionary<Node, Node>();
        //private SortedDictionary<Node, Node> Open = new System.Collections.Generic.SortedDictionary<Node, Node>();
        private List<Node> Open = new List<Node>();
        
        public Puzzle(Color[] colors, Point2D[] solution)
        {
            for(int c = 0; c < SIDES; c++)
            {
                for(int r=0; r < SIDES; r++)
                {
                    Pieces[c,r] = new Piece();
                    Pieces[c,r].Value = colors[c * SIDES + r];
                    Pieces[c,r].CurrentLocation = new Point2D(c, r);
                    Pieces[c,r].StartLocation = new Point2D(c, r);
                    Pieces[c,r].Destination = solution[c * SIDES + r];
                    Pieces[c,r].OnPuzzle = this;
                    Pieces[c,r].Key = (char)(c * SIDES + r + 48);
                    
                    //Console.WriteLine("{0}", c * SIDES + r);
                    OriginalPieces[c * SIDES + r] = Pieces[c,r];
                    //Console.WriteLine("Here");
                    
                    if (Pieces[c,r].IsEmpty) EmptyPiece = Pieces[c,r];
                }
            }
            
            Node root = new Node(this);
            Open.Add(root);
            Best = root;

            //Attempted.Add(ToKey(), true);
        }
        
        public void Draw()
        {
            for(int c = 0; c < SIDES; c++)
            {
                for(int r=0; r < SIDES; r++)
                {
                    Pieces[c,r].Draw();
                }
            }            
        }
        
        public bool StepTowardSolution()
        {
            if(HasSolution()) return true;
            if(Open.Count <= 0) return false;
            if(Quit) return false;
            
            Node next = Open[0];
            Console.WriteLine("Best is: {0} ({1}) of {2} options at depth {3}", next.State, next.Cost, Open.Count, next.Depth);
            
            Open.Remove(next);
            Closed.Add(next, next);
            Best = next;
            
            if(next.Depth > MAX_DEPTH) 
            {
                Quit = true;
                return false;
            }
            
            List<Node> nodes = next.Expand(Closed);
            
            foreach(Node n in nodes)
            {
                if(n.IsAtGoal()) 
                {
                    Best = n;
                    return true;
                }
                
                if(false == Open.Contains(n))
                {
                    //its not in the open list
                    Open.Add(n);
                    Console.WriteLine("Adding {0} ({1})", n.State, n.Cost);
                    Open.Sort();
                }
                else
                {
                    Node other = Open[Open.IndexOf(n)];
                    
                    if(n.Cost < other.Cost) 
                    {
                        Console.WriteLine("Replacing node {0}({1}) with {2}({3})", n.State, n.Cost, other.State, other.Cost);
                        Open.Remove(other);
                        Open.Add(n);
                        Open.Sort();
                    }
                    else 
                    {
                        Console.WriteLine("Skipping... {0} ({1}) -> have {2} ({3})", n.State, n.Cost, other.State, other.Cost);
                    }
                }
            }
            
            return false;
        }
        
        public bool HasSolution()
        {
            return Best.IsAtGoal();
        }
        
        public void Reset()
        {
            int c, r;
            
            for(int i = 0; i < SIDES * SIDES; i++)
            {
                c = i / Puzzle.SIDES;
                r = i - (c * Puzzle.SIDES);
                
                OriginalPieces[i].CurrentLocation = new Point2D(c, r);
                Pieces[c, r] = OriginalPieces[i];
            }
        }
        
        public int TotalManhattenDistance
        {
            get
            {
                int result = 0;

                for(int c = 0; c < SIDES; c++)
                {
                    for(int r=0; r < SIDES; r++)
                    {
                        result += Pieces[c, r].ManhattenDistance;
                    }
                }
                
                return result;                
            }
        }

        public int TotalHValue
        {
            get
            {
                int result = 0;

                for(int c = 0; c < SIDES; c++)
                {
                    for(int r=0; r < SIDES; r++)
                    {
                        result += Pieces[c, r].HValue;
                    }
                }
                
                return result;                
            }
        }

        private StringBuilder Buffer = new StringBuilder(SIDES * SIDES);
        
        public string ToKey()
        {
            Buffer = new StringBuilder(SIDES * SIDES); //Clear?;
            
            for(int c = 0; c < SIDES; c++)
            {
                for(int r=0; r < SIDES; r++)
                {
                    Buffer.Append(Pieces[c, r].Key);
                }
            }
            return Buffer.ToString();
        }
    }
}
