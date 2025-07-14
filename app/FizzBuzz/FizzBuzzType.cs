namespace FizzBuzz;

public abstract class FizzBuzzType
{
    public abstract string Generate(Number number);

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
    public override string Generate(Number number)
    {
        if (IsFizzBuzz(number))
        {
            return "FizzBuzz";
        }
        if (IsFizz(number))
        {
            return "Fizz";
        }
        if (IsBuzz(number))
        {
            return "Buzz";
        }
        return number.ToString();
    }
}

public class FizzBuzzType02 : FizzBuzzType
{
    public override string Generate(Number number)
    {
        return number.ToString();
    }
}

public class FizzBuzzType03 : FizzBuzzType
{
    public override string Generate(Number number)
    {
        if (IsFizzBuzz(number))
        {
            return "FizzBuzz";
        }
        if (IsFizz(number))
        {
            return number.ToString();
        }
        if (IsBuzz(number))
        {
            return number.ToString();
        }
        return number.ToString();
    }
}

public class NullFizzBuzzType : FizzBuzzType
{
    public override string Generate(Number number)
    {
        return "";
    }
}
