import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, PrintStream}
import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.{FizzBuzzType, FizzBuzzType01, FizzBuzzType02, FizzBuzzType03}
import application.{FizzBuzzValueCommand, FizzBuzzListCommand}

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

  "タイプごとに出力を切り替えることができる" should "タイプ1で1を渡したら文字列1を返す" in {
    fizzbuzz.generate(1, 1) shouldEqual "1"
  }

  "タイプ2の場合" should "1を渡したら文字列1を返す" in {
    fizzbuzz.generate(1, 2) shouldEqual "1"
  }

  it should "3を渡したら文字列3を返す" in {
    fizzbuzz.generate(3, 2) shouldEqual "3"
  }

  it should "5を渡したら文字列5を返す" in {
    fizzbuzz.generate(5, 2) shouldEqual "5"
  }

  it should "15を渡したら文字列15を返す" in {
    fizzbuzz.generate(15, 2) shouldEqual "15"
  }

  "タイプ3の場合" should "1を渡したら文字列1を返す" in {
    fizzbuzz.generate(1, 3) shouldEqual "1"
  }

  it should "3を渡したら文字列3を返す" in {
    fizzbuzz.generate(3, 3) shouldEqual "3"
  }

  it should "5を渡したら文字列5を返す" in {
    fizzbuzz.generate(5, 3) shouldEqual "5"
  }

  it should "15を渡したら文字列FizzBuzzを返す" in {
    fizzbuzz.generate(15, 3) shouldEqual "FizzBuzz"
  }

  "それ以外のタイプの場合" should "空文字を返す" in {
    fizzbuzz.generate(1, 4) shouldEqual ""
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
    val result = List("apple", "orange", "pineapple", "strawberry", "apricot").filter(_.startsWith("a"))
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

  "値オブジェクトを理解する" should "同じ数字と値なら等価である" in {
    val value1 = FizzBuzzValue(1, "1")
    val value2 = FizzBuzzValue(1, "1")
    value1 shouldEqual value2
  }

  it should "異なる数字なら等価でない" in {
    val value1 = FizzBuzzValue(1, "1")
    val value2 = FizzBuzzValue(2, "2")
    value1 should not equal value2
  }

  it should "異なる値なら等価でない" in {
    val value1 = FizzBuzzValue(1, "1")
    val value2 = FizzBuzzValue(1, "Fizz")
    value1 should not equal value2
  }

  it should "toString表現が正しい" in {
    val value = FizzBuzzValue(3, "Fizz")
    value.toString shouldEqual "3:Fizz"
  }

  "ファーストクラスコレクションを理解する" should "空のリストを作成できる" in {
    val list = FizzBuzzList(List.empty)
    list.values shouldBe empty
  }

  it should "値を追加できる" in {
    val list = FizzBuzzList(List.empty)
    val value = FizzBuzzValue(1, "1")
    val newList = list.add(value)
    newList.values should have size 1
    newList.get(0) shouldEqual Some(value)
  }

  it should "指定した範囲でリストを作成できる" in {
    val list = new FizzBuzzList(1, 3, FizzBuzzType01)
    list.values should have size 3
    list.get(0) shouldEqual Some(FizzBuzzValue(1, "1"))
    list.get(1) shouldEqual Some(FizzBuzzValue(2, "2"))
    list.get(2) shouldEqual Some(FizzBuzzValue(3, "Fizz"))
  }

  it should "範囲外のインデックスはNoneを返す" in {
    val list = FizzBuzzList(List.empty)
    list.get(0) shouldEqual None
    list.get(-1) shouldEqual None
  }

  it should "toString表現が正しい" in {
    val list = new FizzBuzzList(1, 3, FizzBuzzType01)
    list.toString shouldEqual "1:1,2:2,3:Fizz"
  }

  "Commandパターンを理解する" should "FizzBuzzValueCommandが正しく動作する" in {
    val command = new FizzBuzzValueCommand(FizzBuzzType01)
    command.execute(3) shouldEqual "Fizz"
    command.execute(5) shouldEqual "Buzz"
    command.execute(15) shouldEqual "FizzBuzz"
    command.execute(1) shouldEqual "1"
  }

  it should "FizzBuzzListCommandが正しく動作する" in {
    val command = new FizzBuzzListCommand(FizzBuzzType01)
    val result = command.execute(5)
    result shouldEqual Array("1", "2", "Fizz", "4", "Buzz")
  }

  it should "異なるタイプのCommandが異なる結果を返す" in {
    val command1 = new FizzBuzzValueCommand(FizzBuzzType01)
    val command2 = new FizzBuzzValueCommand(FizzBuzzType02)
    val command3 = new FizzBuzzValueCommand(FizzBuzzType03)
    
    command1.execute(3) shouldEqual "Fizz"
    command2.execute(3) shouldEqual "3"
    command3.execute(3) shouldEqual "3"
    
    command1.execute(15) shouldEqual "FizzBuzz"
    command2.execute(15) shouldEqual "15"
    command3.execute(15) shouldEqual "FizzBuzz"
  }
}
