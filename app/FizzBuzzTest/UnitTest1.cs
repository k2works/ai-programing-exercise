using Xunit;

namespace FizzBuzzTest;

public class FizzBuzzTest
{
    public class 数を文字列にして返す
    {        public class タイプ1の場合
        {
            private readonly FizzBuzz.FizzBuzz _fizzbuzz;

            public タイプ1の場合()
            {
                _fizzbuzz = new FizzBuzz.FizzBuzz(1);
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

            [Fact]
            public void Test_1から100までのFizzBuzz配列を返す()
            {
                _fizzbuzz.GenerateList();
                var result = _fizzbuzz.List;
                
                Assert.Equal("1", result[0]);
                Assert.Equal("Buzz", result[99]); // 100番目は "Buzz"
                Assert.Equal("Fizz", result[2]);  // 3番目は "Fizz"
                Assert.Equal("Buzz", result[4]);  // 5番目は "Buzz"
                Assert.Equal("FizzBuzz", result[14]); // 15番目は "FizzBuzz"
            }
        }

        public class タイプ2の場合
        {
            private readonly FizzBuzz.FizzBuzz _fizzbuzz;

            public タイプ2の場合()
            {
                _fizzbuzz = new FizzBuzz.FizzBuzz(2);
            }

            [Fact]
            public void Test_1を渡したら文字列1を返す()
            {
                Assert.Equal("1", _fizzbuzz.Generate(1));
            }

            [Fact]
            public void Test_3を渡したら文字列3を返す()
            {
                Assert.Equal("3", _fizzbuzz.Generate(3));
            }

            [Fact]
            public void Test_5を渡したら文字列5を返す()
            {
                Assert.Equal("5", _fizzbuzz.Generate(5));
            }

            [Fact]
            public void Test_15を渡したら文字列15を返す()
            {
                Assert.Equal("15", _fizzbuzz.Generate(15));
            }
        }

        public class タイプ3の場合
        {
            private readonly FizzBuzz.FizzBuzz _fizzbuzz;

            public タイプ3の場合()
            {
                _fizzbuzz = new FizzBuzz.FizzBuzz(3);
            }

            [Fact]
            public void Test_1を渡したら文字列1を返す()
            {
                Assert.Equal("1", _fizzbuzz.Generate(1));
            }

            [Fact]
            public void Test_3を渡したら文字列3を返す()
            {
                Assert.Equal("3", _fizzbuzz.Generate(3));
            }

            [Fact]
            public void Test_5を渡したら文字列5を返す()
            {
                Assert.Equal("5", _fizzbuzz.Generate(5));
            }

            [Fact]
            public void Test_15を渡したら文字列FizzBuzzを返す()
            {
                Assert.Equal("FizzBuzz", _fizzbuzz.Generate(15));
            }
        }

        public class それ以外のタイプの場合
        {
            [Fact]
            public void Test_空文字を返す()
            {
                var fizzbuzz = new FizzBuzz.FizzBuzz(4);
                Assert.Equal("", fizzbuzz.Generate(1));
            }
        }
    }
}
