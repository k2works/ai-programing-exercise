// Commandパターンの基底トレイト
trait FizzBuzzCommand[T] {
  def execute(number: Int): T
}

// 値オブジェクトを返すCommand
class FizzBuzzValueCommand(fizzBuzzType: FizzBuzzType) extends FizzBuzzCommand[String] {
  def execute(number: Int): String = 
    fizzBuzzType.generate(number).value
}

// ファーストクラスコレクションを返すCommand  
class FizzBuzzListCommand(fizzBuzzType: FizzBuzzType) extends FizzBuzzCommand[Array[String]] {
  def execute(number: Int): Array[String] = {
    val fizzbuzzList = new FizzBuzzList(1, number, fizzBuzzType)
    fizzbuzzList.toArray
  }
}

case class FizzBuzzValue(number: Int, value: String) {
  override def toString: String = s"$number:$value"
}

case class FizzBuzzList(values: List[FizzBuzzValue]) {
  def this(start: Int, end: Int, fizzBuzzType: FizzBuzzType) = {
    this((start to end).map(fizzBuzzType.generate(_)).toList)
  }
  
  def add(value: FizzBuzzValue): FizzBuzzList = FizzBuzzList(values :+ value)
  
  def get(index: Int): Option[FizzBuzzValue] = values.lift(index)
  
  def toArray: Array[String] = values.map(_.value).toArray
  
  override def toString: String = values.map(_.toString).mkString(",")
}

abstract class FizzBuzzType {
  def generate(number: Int): FizzBuzzValue
  
  protected def isFizz(number: Int): Boolean = number % 3 == 0
  protected def isBuzz(number: Int): Boolean = number % 5 == 0
  protected def isFizzBuzz(number: Int): Boolean = number % 15 == 0
}

object FizzBuzzType01 extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    if (isFizzBuzz(number)) {
      FizzBuzzValue(number, "FizzBuzz")
    } else if (isFizz(number)) {
      FizzBuzzValue(number, "Fizz")
    } else if (isBuzz(number)) {
      FizzBuzzValue(number, "Buzz")
    } else {
      FizzBuzzValue(number, number.toString)
    }
  }
}

object FizzBuzzType02 extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    FizzBuzzValue(number, number.toString)
  }
}

object FizzBuzzType03 extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    if (isFizzBuzz(number)) {
      FizzBuzzValue(number, "FizzBuzz")
    } else {
      FizzBuzzValue(number, number.toString)
    }
  }
}

class FizzBuzz(private val fizzBuzzType: FizzBuzzType) {
  
  def generate(number: Int): FizzBuzzValue = 
    fizzBuzzType.generate(number)

  def createList(start: Int, end: Int): FizzBuzzList =
    new FizzBuzzList(start, end, fizzBuzzType)
}

object FizzBuzz {
  def generate(number: Int, fizzBuzzType: Int = 1): String = {
    val typeStrategy = fizzBuzzType match {
      case 1 => FizzBuzzType01
      case 2 => FizzBuzzType02
      case 3 => FizzBuzzType03
      case _ => throw new RuntimeException("引数は1から3までです")
    }
    val command = new FizzBuzzValueCommand(typeStrategy)
    command.execute(number)
  }
  
  def createList(start: Int, end: Int): Array[String] = {
    val command = new FizzBuzzListCommand(FizzBuzzType01)
    command.execute(end)
  }
}
