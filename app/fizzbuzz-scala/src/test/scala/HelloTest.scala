import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloTest extends AnyFlatSpec with Matchers {
  "greeting" should "return hello world" in {
    greeting shouldEqual "hello world"
  }
  
  def greeting: String = "hello world"
}
