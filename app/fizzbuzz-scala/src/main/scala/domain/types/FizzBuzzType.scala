package domain.types

import domain.model.FizzBuzzValue

// 代数的データ型として定義
sealed trait FizzBuzzType {
  def generate(number: Int): FizzBuzzValue
}

// コンパニオンオブジェクトで関数型アプローチ
object FizzBuzzType {
  // 型を表現する代数的データ型
  case object Type01 extends FizzBuzzType {
    def generate(number: Int): FizzBuzzValue = 
      FizzBuzzValue(number, generateValue(number))
    
    private def generateValue(number: Int): String = number match {
      case n if isFizzBuzz(n) => "FizzBuzz"
      case n if isFizz(n) => "Fizz"
      case n if isBuzz(n) => "Buzz"
      case n => n.toString
    }
  }
  
  case object Type02 extends FizzBuzzType {
    def generate(number: Int): FizzBuzzValue = 
      FizzBuzzValue(number, number.toString)
  }
  
  case object Type03 extends FizzBuzzType {
    def generate(number: Int): FizzBuzzValue = 
      FizzBuzzValue(number, generateValue(number))
    
    private def generateValue(number: Int): String = number match {
      case n if isFizzBuzz(n) => "FizzBuzz"
      case n => n.toString
    }
  }
  
  case object NotDefined extends FizzBuzzType {
    def generate(number: Int): FizzBuzzValue = 
      FizzBuzzValue(number, "")
  }
  
  // 純粋関数として定義
  private def isFizz(number: Int): Boolean = number % 3 == 0
  private def isBuzz(number: Int): Boolean = number % 5 == 0  
  private def isFizzBuzz(number: Int): Boolean = number % 15 == 0
  
  // ファクトリ関数
  def create(typeNum: Int): FizzBuzzType = typeNum match {
    case 1 => Type01
    case 2 => Type02
    case 3 => Type03
    case _ => NotDefined
  }
  
  // 型安全な列挙
  val allTypes: List[FizzBuzzType] = List(Type01, Type02, Type03, NotDefined)
}
