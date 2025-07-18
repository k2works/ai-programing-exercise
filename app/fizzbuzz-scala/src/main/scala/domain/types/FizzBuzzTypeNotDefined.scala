package domain.types

import domain.model.FizzBuzzValue

object FizzBuzzTypeNotDefined extends FizzBuzzType {
  override def generate(number: Int): FizzBuzzValue = {
    FizzBuzzValue(number, "")
  }
  
  override def toString: String = "未定義"
}
