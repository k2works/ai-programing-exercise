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
        Assert.Equal("1", _fizzbuzz.Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", _fizzbuzz.Generate(2));
    }

    [Fact]
    public void Test_3を渡したら文字列Fizzを返す()
    {
        Assert.Equal("Fizz", _fizzbuzz.Generate(3));
    }

    [Fact]
    public void Test_5を渡したら文字列Buzzを返す()
    {
        Assert.Equal("Buzz", _fizzbuzz.Generate(5));
    }

    [Fact]
    public void Test_15を渡したら文字列FizzBuzzを返す()
    {
        Assert.Equal("FizzBuzz", _fizzbuzz.Generate(15));
    }
}