package domain.model

// 純粋な不変データ構造として定義
case class FizzBuzzList private (values: List[FizzBuzzValue]) {
  // 不変な操作のみ提供
  def add(value: FizzBuzzValue): FizzBuzzList = 
    FizzBuzzList.create(values :+ value)
  
  def get(index: Int): Option[FizzBuzzValue] = 
    values.lift(index)
  
  def toArray: Array[String] = 
    values.map(_.value).toArray
  
  def map[B](f: FizzBuzzValue => B): List[B] = 
    values.map(f)
  
  def filter(predicate: FizzBuzzValue => Boolean): FizzBuzzList = 
    FizzBuzzList.create(values.filter(predicate))
  
  def foldLeft[B](z: B)(op: (B, FizzBuzzValue) => B): B = 
    values.foldLeft(z)(op)
  
  def size: Int = values.length
  
  override def toString: String = 
    values.map(_.toString).mkString(",")
}

object FizzBuzzList {
  val MaxCount = 100
  
  // スマートコンストラクタで検証
  def create(values: List[FizzBuzzValue]): FizzBuzzList = {
    require(values.length <= MaxCount, s"上限は${MaxCount}件までです")
    new FizzBuzzList(values)
  }
  
  // 関数型的なファクトリメソッド
  def generate(start: Int, end: Int, fizzBuzzType: domain.types.FizzBuzzType): FizzBuzzList = {
    val values = (start to end).map(fizzBuzzType.generate).toList
    create(values)
  }
  
  // 空のリスト
  def empty: FizzBuzzList = 
    new FizzBuzzList(List.empty)
  
  // 単一要素のリスト
  def single(value: FizzBuzzValue): FizzBuzzList = 
    create(List(value))
  
  // 関数合成を使用したビルダー
  def builder: FizzBuzzListBuilder = 
    new FizzBuzzListBuilder(List.empty)
}

// 関数型ビルダーパターン
class FizzBuzzListBuilder private[model] (private val values: List[FizzBuzzValue]) {
  def add(value: FizzBuzzValue): FizzBuzzListBuilder = 
    new FizzBuzzListBuilder(values :+ value)
  
  def addAll(newValues: List[FizzBuzzValue]): FizzBuzzListBuilder = 
    new FizzBuzzListBuilder(values ++ newValues)
  
  def build: FizzBuzzList = 
    FizzBuzzList.create(values)
}
