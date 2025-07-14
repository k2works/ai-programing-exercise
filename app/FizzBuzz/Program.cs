using System;
using FizzBuzz;

class Program
{
    static void Main(string[] args)
    {
        var fizzbuzz = new FizzBuzz.FizzBuzz();
        
        for (int i = 1; i <= 100; i++)
        {
            Console.WriteLine(fizzbuzz.Generate(i));
        }
    }
}
