using System;
using FizzBuzz;

sealed class Program
{
    static void Main(string[] args)
    {
        var fizzbuzz = new FizzBuzz.FizzBuzz();

        for (int i = 1; i <= 100; i++)
        {
            Console.WriteLine(FizzBuzz.FizzBuzz.Generate(i));
        }
    }
}
