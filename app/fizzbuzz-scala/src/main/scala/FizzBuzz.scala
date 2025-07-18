case class FizzBuzzValue(number: Int, value: String) {
  override def toString: String = s"$number:$value"
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

  def createList(start: Int, end: Int): Array[FizzBuzzValue] =
    (start to end).map(generate(_)).toArray
}

object FizzBuzz {
  def generate(number: Int, fizzBuzzType: Int = 1): String = {
    val typeStrategy = fizzBuzzType match {
      case 1 => FizzBuzzType01
      case 2 => FizzBuzzType02
      case 3 => FizzBuzzType03
      case _ => throw new RuntimeException("引数は1から3までです")
    }
    val fizzbuzz = new FizzBuzz(typeStrategy)
    fizzbuzz.generate(number).value
  }
  
  def createList(start: Int, end: Int): Array[String] = {
    val fizzbuzz = new FizzBuzz(FizzBuzzType01)
    fizzbuzz.createList(start, end).map(_.value)
  }
}
