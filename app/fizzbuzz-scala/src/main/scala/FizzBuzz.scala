trait FizzBuzzType {
  def generate(number: Int): String
}

object FizzBuzzType01 extends FizzBuzzType {
  def generate(number: Int): String = {
    if (number % 15 == 0) {
      "FizzBuzz"
    } else if (number % 3 == 0) {
      "Fizz"
    } else if (number % 5 == 0) {
      "Buzz"
    } else {
      number.toString
    }
  }
}

object FizzBuzzType02 extends FizzBuzzType {
  def generate(number: Int): String = {
    number.toString
  }
}

object FizzBuzzType03 extends FizzBuzzType {
  def generate(number: Int): String = {
    if (number % 15 == 0) {
      "FizzBuzz"
    } else {
      number.toString
    }
  }
}

class FizzBuzz(private val fizzBuzzType: FizzBuzzType) {
  
  def generate(number: Int): String = 
    fizzBuzzType.generate(number)

  def createList(start: Int, end: Int): Array[String] =
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
    fizzbuzz.generate(number)
  }
  
  def createList(start: Int, end: Int): Array[String] = {
    val fizzbuzz = new FizzBuzz(FizzBuzzType01)
    fizzbuzz.createList(start, end)
  }
}
