class FizzBuzz(private val fizzBuzzType: Int = 1) {
  
  def generate(number: Int): String = 
    fizzBuzzType match {
      case 1 =>
        if (number % 15 == 0) {
          "FizzBuzz"
        } else if (number % 3 == 0) {
          "Fizz"
        } else if (number % 5 == 0) {
          "Buzz"
        } else {
          number.toString
        }
      case 2 =>
        number.toString
      case 3 =>
        if (number % 15 == 0) {
          "FizzBuzz"
        } else {
          number.toString
        }
      case _ =>
        throw new RuntimeException("引数は1から3までです")
    }

  def createList(start: Int, end: Int): Array[String] =
    (start to end).map(generate(_)).toArray
}

object FizzBuzz {
  def generate(number: Int, fizzBuzzType: Int = 1): String = {
    val fizzbuzz = new FizzBuzz(fizzBuzzType)
    fizzbuzz.generate(number)
  }
  
  def createList(start: Int, end: Int): Array[String] = {
    val fizzbuzz = new FizzBuzz(1)
    fizzbuzz.createList(start, end)
  }
}
