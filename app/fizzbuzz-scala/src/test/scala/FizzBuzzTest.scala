import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FizzBuzzTest extends AnyFlatSpec with Matchers {
  val fizzbuzz = FizzBuzz

  "generate" should "1を渡したら文字列1を返す" in {
    fizzbuzz.generate(1) shouldEqual "1"
  }

  it should "2を渡したら文字列2を返す" in {
    fizzbuzz.generate(2) shouldEqual "2"
  }

  it should "3を渡したら文字列Fizzを返す" in {
    fizzbuzz.generate(3) shouldEqual "Fizz"
  }

  it should "5を渡したら文字列Buzzを返す" in {
    fizzbuzz.generate(5) shouldEqual "Buzz"
  }

  it should "15を渡したら文字列FizzBuzzを返す" in {
    fizzbuzz.generate(15) shouldEqual "FizzBuzz"
  }
}
