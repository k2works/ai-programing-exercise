namespace FizzBuzz;

public abstract class FizzBuzzType
{
    public abstract FizzBuzzValue Generate(Number number);

    protected bool IsFizz(Number number)
    {
        return number.IsFizz();
    }

    protected bool IsBuzz(Number number)
    {
        return number.IsBuzz();
    }

    protected bool IsFizzBuzz(Number number)
    {
        return number.IsFizzBuzz();
    }
}

public class FizzBuzzType01 : FizzBuzzType
{
    public override FizzBuzzValue Generate(Number number)
    {
        if (IsFizzBuzz(number))
        {
            return FizzBuzzValue.FizzBuzz();
        }
        if (IsFizz(number))
        {
            return FizzBuzzValue.Fizz();
        }
        if (IsBuzz(number))
        {
            return FizzBuzzValue.Buzz();
        }
        return FizzBuzzValue.Number(number.Value);
    }
}

public class FizzBuzzType02 : FizzBuzzType
{
    public override FizzBuzzValue Generate(Number number)
    {
        return FizzBuzzValue.Number(number.Value);
    }
}

public class FizzBuzzType03 : FizzBuzzType
{
    public override FizzBuzzValue Generate(Number number)
    {
        if (IsFizzBuzz(number))
        {
            return FizzBuzzValue.FizzBuzz();
        }
        if (IsFizz(number))
        {
            return FizzBuzzValue.Number(number.Value);
        }
        if (IsBuzz(number))
        {
            return FizzBuzzValue.Number(number.Value);
        }
        return FizzBuzzValue.Number(number.Value);
    }
}

public class NullFizzBuzzType : FizzBuzzType
{
    public override FizzBuzzValue Generate(Number number)
    {
        return FizzBuzzValue.Empty();
    }
}
