// See https://aka.ms/new-console-template for more information

//no main method???


using System.Runtime.CompilerServices;
using System;
using System.Collections;
using day6;

internal class Program
{
    public static bool PositionIsValid(Pair<int> position, ArrayList matrix)
    {
        //this is your brain on Scheme
        return (position.First() > 0 && position.First() < (matrix[0] as Cell[]).Length) && (position.Second() > 0 && position.Second() < matrix.Count);
    }

    private static void Main(string[] args)
    {
        try
        {
            using StreamReader r = new("../../../input");



            ArrayList matrix = new ArrayList();
            //position.First() -> x, position.Second() -> y.
            Pair<int> position = new(0, 0);
            Direction direction = Direction.Up;
            int rc = 0;


            // plain c-style
            string line;
            while ((line = r.ReadLine()) != null)
            {
                //Console.WriteLine(r.ReadLine());
                rc++;
                Cell[] cells = new Cell[line.Length];
                for (int i = 0; i < line.Length; i++)
                {
                    if (line[i].Equals('^'))
                    {
                        position = new(rc - 1, i);  //fucking stupid, why is this allowed when new Pair<int> is 100x more readable??
                    }
                    bool isObstacle = !line[i].Equals('.');
                    cells[i] = new Cell(isObstacle);
                }

                matrix.Add(cells);


            }
            //matrix is constructed.


            Console.WriteLine("matrix length: {0}", matrix.Count);
            Console.WriteLine("player position: ({0},{1})", position.First(), position.Second());

            //making money moves
            while (PositionIsValid(position, matrix))
            {
                (matrix[position.First()] as Cell[])[position.Second()].Visit(); // I've gotta figure out this typed AL thing in c#, it's so easy in java :(
                //check obstacle
                

                //finally, move
                switch(direction)
                {
                    case Direction.Up:
                        position.SetFirst(position.First() - 1);
                        break;
                    case Direction.Right:
                        position.SetSecond(position.Second() + 1);
                        break;
                    case Direction.Down:
                        position.SetFirst(position.First() + 1);
                        break;
                    case Direction.Left:
                        position.SetSecond(position.Second() - 1);
                        break;
                }
            }
            
            //tallying moved tiles

        }
        catch (IOException e)
        {
            Console.WriteLine("could not read file;");
            Console.WriteLine(e.Message);
        }
        catch (Exception e)
        {
            Console.OpenStandardError();
            Console.WriteLine(e.Message);
        }
    }


}