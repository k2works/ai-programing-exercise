package application

import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.FizzBuzzType

// 関数型アプローチ - 純粋関数として実装
object FizzBuzzCommand {
  
  // 単一値を生成する関数
  def generateValue(number: Int)(fizzBuzzType: FizzBuzzType): String = 
    fizzBuzzType.generate(number).value
  
  // リストを生成する関数  
  def generateList(start: Int, end: Int)(fizzBuzzType: FizzBuzzType): Array[String] = 
    FizzBuzzList.generate(start, end, fizzBuzzType).toArray
  
  // 高階関数 - 変換関数を受け取る
  def transform[A, B](range: Range)(fizzBuzzType: FizzBuzzType)(f: FizzBuzzValue => B): List[B] = 
    range.map(fizzBuzzType.generate).map(f).toList
  
  // カリー化された関数
  def processRange(fizzBuzzType: FizzBuzzType): Range => List[FizzBuzzValue] = 
    range => range.map(fizzBuzzType.generate).toList
  
  // 関数合成
  def compose[A, B, C](f: B => C)(g: A => B): A => C = 
    a => f(g(a))
  
  // パーティアル適用のヘルパー
  def withType(fizzBuzzType: FizzBuzzType): FizzBuzzOperations = 
    new FizzBuzzOperations(fizzBuzzType)
}

// 関数型操作のラッパー
class FizzBuzzOperations(fizzBuzzType: FizzBuzzType) {
  def single(number: Int): String = 
    FizzBuzzCommand.generateValue(number)(fizzBuzzType)
  
  def range(start: Int, end: Int): Array[String] = 
    FizzBuzzCommand.generateList(start, end)(fizzBuzzType)
  
  def mapRange[B](start: Int, end: Int)(f: FizzBuzzValue => B): List[B] = 
    FizzBuzzCommand.transform(start to end)(fizzBuzzType)(f)
}
