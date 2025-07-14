using System;
using FizzBuzz;

class Program
{
    static void Main(string[] args)
    {
        try
        {
            var app = new FizzBuzzApplication(1);
            app.Run();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"エラーが発生しました: {ex.Message}");
        }
    }
}
