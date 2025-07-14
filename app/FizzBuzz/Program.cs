using System;
using FizzBuzz;

class Program
{
    static void Main(string[] args)
    {
        var fizzbuzz = new FizzBuzz.FizzBuzz();
        
        fizzbuzz.GenerateList();
        
        foreach (string result in fizzbuzz.List)
        {
            Console.WriteLine(result);
        }
    }
}
