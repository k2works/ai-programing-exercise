import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.FizzBuzzType
import application.FizzBuzzCommand

// 純粋関数型アプローチ - クラスではなく関数として設計
object FizzBuzz {
  
  // 基本的な生成関数
  def generate(number: Int, typeNum: Int = 1): String = {
    val fizzBuzzType = FizzBuzzType.create(typeNum)
    FizzBuzzCommand.generateValue(number)(fizzBuzzType)
  }
  
  // リスト生成関数
  def createList(start: Int, end: Int, typeNum: Int = 1): Array[String] = {
    val fizzBuzzType = FizzBuzzType.create(typeNum)
    FizzBuzzCommand.generateList(start, end)(fizzBuzzType)
  }
  
  // 高階関数アプローチ
  def generateWith(fizzBuzzType: FizzBuzzType): Int => String = 
    number => FizzBuzzCommand.generateValue(number)(fizzBuzzType)
  
  // 範囲処理の関数
  def processRange(start: Int, end: Int, fizzBuzzType: FizzBuzzType): List[FizzBuzzValue] = 
    (start to end).map(fizzBuzzType.generate).toList
  
  // 関数合成を使った変換
  def transform[B](start: Int, end: Int, typeNum: Int = 1)(f: FizzBuzzValue => B): List[B] = {
    val fizzBuzzType = FizzBuzzType.create(typeNum)
    FizzBuzzCommand.transform(start to end)(fizzBuzzType)(f)
  }
  
  // モナディックスタイル（Option使用）
  def safeGenerate(number: Int, typeNum: Int): Option[String] = {
    if (number >= 0) {
      Some(generate(number, typeNum))
    } else {
      None
    }
  }
  
  // Either型を使ったエラーハンドリング
  def generateEither(number: Int, typeNum: Int): Either[String, String] = {
    if (number < 0) {
      Left("負の数は処理できません")
    } else {
      Right(generate(number, typeNum))
    }
  }
  
  // ストリーミング処理
  def stream(start: Int, typeNum: Int = 1): LazyList[String] = 
    LazyList.from(start).map(generate(_, typeNum))
  
  // 述語に基づくフィルタリング
  def generateWithFilter(start: Int, end: Int, typeNum: Int = 1)(predicate: FizzBuzzValue => Boolean): List[String] = {
    val fizzBuzzType = FizzBuzzType.create(typeNum)
    (start to end)
      .map(fizzBuzzType.generate)
      .filter(predicate)
      .map(_.value)
      .toList
  }
}

// 既存のAPIとの互換性のためのラッパー（非推奨）
@deprecated("Use functional approach instead", "2.0.0")
class FizzBuzz(private val fizzBuzzType: FizzBuzzType) {
  
  def generate(number: Int): FizzBuzzValue = 
    fizzBuzzType.generate(number)

  def createList(start: Int, end: Int): FizzBuzzList =
    FizzBuzzList.generate(start, end, fizzBuzzType)
}
