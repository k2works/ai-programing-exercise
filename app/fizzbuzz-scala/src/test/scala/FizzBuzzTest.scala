import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FizzBuzzTest extends AnyFlatSpec with Matchers {
  "generate" should "1を渡したら文字列1を返す" in {
    FizzBuzz.generate(1) shouldEqual "1"
  }

  it should "2を渡したら文字列2を返す" in {
    FizzBuzz.generate(2) shouldEqual "2"
  }
}
