using Xunit;

namespace FizzBuzzTest;

public class FizzBuzzTest
{
    public class 数を文字列にして返す
    {
        public class タイプ1の場合
        {
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

        public class タイプ2の場合
        {
            [Fact]
            public void Test_1を渡したら文字列1を返す()
            {
                Assert.Equal("1", FizzBuzz.FizzBuzz.Generate(1, 2));
            }

            [Fact]
            public void Test_3を渡したら文字列3を返す()
            {
                Assert.Equal("3", FizzBuzz.FizzBuzz.Generate(3, 2));
            }

            [Fact]
            public void Test_5を渡したら文字列5を返す()
            {
                Assert.Equal("5", FizzBuzz.FizzBuzz.Generate(5, 2));
            }

            [Fact]
            public void Test_15を渡したら文字列15を返す()
            {
                Assert.Equal("15", FizzBuzz.FizzBuzz.Generate(15, 2));
            }
        }
        public class タイプ3の場合
        {
            [Fact]
            public void Test_1を渡したら文字列1を返す()
            {
                Assert.Equal("1", FizzBuzz.FizzBuzz.Generate(1, 3));
            }

            [Fact]
            public void Test_3を渡したら文字列3を返す()
            {
                Assert.Equal("3", FizzBuzz.FizzBuzz.Generate(3, 3));
            }

            [Fact]
            public void Test_5を渡したら文字列5を返す()
            {
                Assert.Equal("5", FizzBuzz.FizzBuzz.Generate(5, 3));
            }

            [Fact]
            public void Test_15を渡したら文字列FizzBuzzを返す()
            {
                Assert.Equal("FizzBuzz", FizzBuzz.FizzBuzz.Generate(15, 3));
            }
        }
    }
}
