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
}