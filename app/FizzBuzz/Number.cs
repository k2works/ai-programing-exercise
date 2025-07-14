namespace FizzBuzz;

public class Number
{
    private readonly int _value;

    public Number(int value)
    {
        _value = value;
    }

    public int Value => _value;

    public bool IsFizz()
    {
        return _value % 3 == 0;
    }

    public bool IsBuzz()
    {
        return _value % 5 == 0;
    }

    public bool IsFizzBuzz()
    {
        return IsFizz() && IsBuzz();
    }

    public override string ToString()
    {
        return _value.ToString();
    }

    public override bool Equals(object obj)
    {
        if (obj is Number other)
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
