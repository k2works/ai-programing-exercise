import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, PrintStream}
import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.FizzBuzzType
import application.FizzBuzzCommand

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
    val list = FizzBuzzList.empty
    list.size shouldBe 0
  }

  it should "値を追加できる" in {
    val list = FizzBuzzList.empty
    val value = FizzBuzzValue(1, "1")
    val newList = list.add(value)
    newList.size shouldEqual 1
    newList.get(0) shouldEqual Some(value)
  }

  it should "指定した範囲でリストを作成できる" in {
    val list = FizzBuzzList.generate(1, 3, FizzBuzzType.Type01)
    list.size shouldEqual 3
    list.get(0) shouldEqual Some(FizzBuzzValue(1, "1"))
    list.get(1) shouldEqual Some(FizzBuzzValue(2, "2"))
    list.get(2) shouldEqual Some(FizzBuzzValue(3, "Fizz"))
  }

  it should "範囲外のインデックスはNoneを返す" in {
    val list = FizzBuzzList.empty
    list.get(0) shouldEqual None
    list.get(-1) shouldEqual None
  }

  it should "toString表現が正しい" in {
    val list = FizzBuzzList.generate(1, 3, FizzBuzzType.Type01)
    list.toString shouldEqual "1:1,2:2,3:Fizz"
  }

  // 関数型プログラミングのテスト
  "高階関数" should "generateWithを使用して特定の型で関数を生成する" in {
    val generateType1 = fizzbuzz.generateWith(FizzBuzzType.Type01)
    generateType1(3) shouldEqual "Fizz"
    generateType1(5) shouldEqual "Buzz"
    generateType1(15) shouldEqual "FizzBuzz"
  }

  "関数合成" should "transformを使用して変換関数を適用する" in {
    val lengths = fizzbuzz.transform(1, 5)(_.value.length)
    lengths shouldEqual List(1, 1, 4, 1, 4) // "1", "2", "Fizz", "4", "Buzz"
  }

  "Option型" should "safeGenerateで安全な処理を行う" in {
    fizzbuzz.safeGenerate(3, 1) shouldEqual Some("Fizz")
    fizzbuzz.safeGenerate(-1, 1) shouldEqual None
  }

  "Either型" should "generateEitherでエラーハンドリングを行う" in {
    fizzbuzz.generateEither(3, 1) shouldEqual Right("Fizz")
    fizzbuzz.generateEither(-1, 1) shouldEqual Left("負の数は処理できません")
  }

  "遅延評価" should "streamで無限ストリームを生成する" in {
    val first5 = fizzbuzz.stream(1).take(5).toList
    first5 shouldEqual List("1", "2", "Fizz", "4", "Buzz")
  }

  "フィルタリング" should "generateWithFilterで述語に基づいてフィルタリングする" in {
    val fizzBuzzOnly = fizzbuzz.generateWithFilter(1, 15)(_.value.contains("z"))
    fizzBuzzOnly shouldEqual List("Fizz", "Buzz", "Fizz", "Fizz", "Buzz", "Fizz", "FizzBuzz")
  }

  "FizzBuzzCommand関数型" should "generateValueで単一値を生成する" in {
    val result = FizzBuzzCommand.generateValue(3)(FizzBuzzType.Type01)
    result shouldEqual "Fizz"
  }

  it should "generateListでリストを生成する" in {
    val result = FizzBuzzCommand.generateList(1, 5)(FizzBuzzType.Type01)
    result shouldEqual Array("1", "2", "Fizz", "4", "Buzz")
  }

  it should "withTypeでパーティアル適用を行う" in {
    val operations = FizzBuzzCommand.withType(FizzBuzzType.Type01)
    operations.single(3) shouldEqual "Fizz"
    operations.range(1, 5) shouldEqual Array("1", "2", "Fizz", "4", "Buzz")
  }

  "FizzBuzzList関数型" should "builderパターンで構築する" in {
    val list = FizzBuzzList.builder
      .add(FizzBuzzValue(1, "1"))
      .add(FizzBuzzValue(3, "Fizz"))
      .build
    
    list.size shouldEqual 2
    list.get(1) shouldEqual Some(FizzBuzzValue(3, "Fizz"))
  }

  it should "関数型操作をサポートする" in {
    val list = FizzBuzzList.generate(1, 5, FizzBuzzType.Type01)
    
    val lengths = list.map(_.value.length)
    lengths shouldEqual List(1, 1, 4, 1, 4)
    
    val filtered = list.filter(_.value.contains("z"))
    filtered.size shouldEqual 2 // "Fizz", "Buzz"
    
    val sum = list.foldLeft(0)(_ + _.number)
    sum shouldEqual 15 // 1+2+3+4+5
  }
}
