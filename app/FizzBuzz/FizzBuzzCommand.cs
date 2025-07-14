namespace FizzBuzz;

public interface IFizzBuzzCommand
{
    FizzBuzzValue Execute(Number number);
}

public class FizzBuzzCommand : IFizzBuzzCommand
{
    private readonly FizzBuzzType _type;

    public FizzBuzzCommand(FizzBuzzType type)
    {
        _type = type;
    }

    public FizzBuzzValue Execute(Number number)
    {
        return _type.Generate(number);
    }
}
