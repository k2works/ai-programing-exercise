namespace FizzBuzz;

public abstract class FizzBuzzType
{
    public abstract string Generate(int number);

    protected bool IsFizz(int number)
    {
        return number % 3 == 0;
    }

    protected bool IsBuzz(int number)
    {
        return number % 5 == 0;
    }
}

public class FizzBuzzType01 : FizzBuzzType
{
    public override string Generate(int number)
    {
        if (IsFizz(number) && IsBuzz(number))
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
    public override string Generate(int number)
    {
        return number.ToString();
    }
}

public class FizzBuzzType03 : FizzBuzzType
{
    public override string Generate(int number)
    {
        if (IsFizz(number) && IsBuzz(number))
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
