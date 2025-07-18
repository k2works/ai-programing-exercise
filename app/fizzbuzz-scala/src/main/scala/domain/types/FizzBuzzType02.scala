package domain.types

import domain.model.FizzBuzzValue

object FizzBuzzType02 extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    FizzBuzzValue(number, number.toString)
  }
}
