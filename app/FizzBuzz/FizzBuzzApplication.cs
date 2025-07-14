namespace FizzBuzz;

public class FizzBuzzApplication
{
    private readonly FizzBuzz _fizzBuzz;

    public FizzBuzzApplication(int type)
    {
        _fizzBuzz = new FizzBuzz(type);
    }

    public void Run()
    {
        Console.WriteLine("FizzBuzzアプリケーション実行:");
        
        _fizzBuzz.GenerateList();
        var results = _fizzBuzz.List;
        
        for (int i = 0; i < results.Count; i++)
        {
            Console.WriteLine($"{i + 1}: {results[i]}");
        }
    }

    public string GetSingleResult(int number)
    {
        return _fizzBuzz.Generate(number);
    }
}
