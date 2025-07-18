import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, PrintStream}

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
    val expected = Array(
      "1",
      "2",
      "Fizz",
      "4",
      "Buzz",
      "Fizz",
      "7",
      "8",
      "Fizz",
      "Buzz",
      "11",
      "Fizz",
      "13",
      "14",
      "FizzBuzz"
    )
    fizzbuzz.createList(1, 15) should contain theSameElementsInOrderAs expected
  }

  "配列やコレクション操作を理解する" should "繰り返し処理" in {
    val results = List(1, 2, 3).map(i => i * i)
    results shouldEqual List(1, 4, 9)
  }

  it should "filterメソッドで特定の条件を満たす要素だけを配列に入れて返す" in {
    val result = List(1.1, 2.0, 3.3, 4.0).filter(_.isWhole)
    result shouldEqual List(2.0, 4.0)
  }

  it should "特定の条件を満たさない要素だけを配列に入れて返す" in {
    val result = List(1.1, 2.0, 3.3, 4.0).filterNot(_.isWhole)
    result shouldEqual List(1.1, 3.3)
  }

  it should "mapメソッドで新しい要素の配列を返す" in {
    val result = List("apple", "orange", "pineapple", "strawberry").map(_.length)
    result shouldEqual List(5, 6, 9, 10)
  }

  it should "findメソッドで配列の中から条件に一致する要素を取得する" in {
    val result = List("apple", "orange", "pineapple", "strawberry").find(_.nonEmpty)
    result shouldEqual Some("apple")
  }

  it should "指定した評価式で並び変えた配列を返す" in {
    val result1 = List("2", "4", "13", "3", "1", "10").sorted
    val result2 = List("2", "4", "13", "3", "1", "10").sortBy(_.toInt)
    val result3 = List("2", "4", "13", "3", "1", "10").sortBy(_.toInt)(Ordering.Int.reverse)

    result1 shouldEqual List("1", "10", "13", "2", "3", "4")
    result2 shouldEqual List("1", "2", "3", "4", "10", "13")
    result3 shouldEqual List("13", "10", "4", "3", "2", "1")
  }

  it should "正規表現で配列の中から条件に一致する要素を取得する" in {
    val result =
      List("apple", "orange", "pineapple", "strawberry", "apricot").filter(_.startsWith("a"))
    result shouldEqual List("apple", "apricot")
  }

  it should "ブロック内の条件式が真である間までの要素を返す" in {
    val result = List(1, 2, 3, 4, 5, 6, 7, 8, 9).takeWhile(_ < 6)
    result shouldEqual List(1, 2, 3, 4, 5)
  }

  it should "ブロック内の条件式が真である以降の要素を返す" in {
    val result = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).dropWhile(_ < 6)
    result shouldEqual List(6, 7, 8, 9, 10)
  }

  it should "foldLeftメソッドで畳み込み演算を行う" in {
    val result = List(1, 2, 3, 4, 5).foldLeft(0)(_ + _)
    result shouldEqual 15
  }

  it should "reduceオプションメソッドで畳み込み演算を行う" in {
    val result = List(1, 2, 3, 4, 5).reduceOption(_ + _)
    result shouldEqual Some(15)
  }
}
