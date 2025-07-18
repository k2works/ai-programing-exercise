package domain.types

import domain.model.FizzBuzzValue

object FizzBuzzType03 extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    if (isFizzBuzz(number)) {
      FizzBuzzValue(number, "FizzBuzz")
    } else {
      FizzBuzzValue(number, number.toString)
    }
  }
}
