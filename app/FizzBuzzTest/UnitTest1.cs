using Xunit;

namespace FizzBuzzTest;

public class FizzBuzzTest
{
    private FizzBuzz.FizzBuzz _fizzbuzz;

    public FizzBuzzTest()
    {
        _fizzbuzz = new FizzBuzz.FizzBuzz();
    }

    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", FizzBuzz.FizzBuzz.Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", FizzBuzz.FizzBuzz.Generate(2));
    }

    [Fact]
    public void Test_3を渡したら文字列Fizzを返す()
    {
        Assert.Equal("Fizz", FizzBuzz.FizzBuzz.Generate(3));
    }

    [Fact]
    public void Test_5を渡したら文字列Buzzを返す()
    {
        Assert.Equal("Buzz", FizzBuzz.FizzBuzz.Generate(5));
    }

    [Fact]
    public void Test_15を渡したら文字列FizzBuzzを返す()
    {
        Assert.Equal("FizzBuzz", FizzBuzz.FizzBuzz.Generate(15));
    }
}
