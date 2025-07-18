object FizzBuzz {
  def generate(number: Int, fizzBuzzType: Int = 1): String =
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
    }

  def createList(start: Int, end: Int): Array[String] =
    (start to end).map(generate(_, 1)).toArray
}
