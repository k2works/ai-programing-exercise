namespace FizzBuzz;

public interface IFizzBuzzCommand
{
    string Execute(Number number);
}

public class FizzBuzzCommand : IFizzBuzzCommand
{
    private readonly FizzBuzzType _type;

    public FizzBuzzCommand(FizzBuzzType type)
    {
        _type = type;
    }

    public string Execute(Number number)
    {
        return _type.Generate(number);
    }
}
