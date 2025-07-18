package domain.types

import domain.model.FizzBuzzValue

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
