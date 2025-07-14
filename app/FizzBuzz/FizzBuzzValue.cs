namespace FizzBuzz;

public class FizzBuzzValue
{
    private readonly string _value;

    private FizzBuzzValue(string value)
    {
        _value = value;
    }

    public static FizzBuzzValue Fizz() => new FizzBuzzValue("Fizz");
    public static FizzBuzzValue Buzz() => new FizzBuzzValue("Buzz");
    public static FizzBuzzValue FizzBuzz() => new FizzBuzzValue("FizzBuzz");
    public static FizzBuzzValue Number(int number) => new FizzBuzzValue(number.ToString());
    public static FizzBuzzValue Empty() => new FizzBuzzValue("");

    public override string ToString()
    {
        return _value;
    }

    public override bool Equals(object obj)
    {
        if (obj is FizzBuzzValue other)
        {
            return _value == other._value;
        }
        return false;
    }

    public override int GetHashCode()
    {
        return _value.GetHashCode();
    }
}
