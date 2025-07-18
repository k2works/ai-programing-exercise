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

  it should "1から15までのFizzBuzzの配列を返す" in {
    val expected = Array("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz")
    fizzbuzz.createList(1, 15) should contain theSameElementsInOrderAs expected
  }
}
