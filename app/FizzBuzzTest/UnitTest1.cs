using Xunit;

namespace FizzBuzzTest;

public class HelloTest
{
    [Fact]
    public void Test_Greeting()
    {
        Assert.Equal("hello world", Greeting());
    }
    
    private string Greeting()
    {
        return "hello world";
    }
}