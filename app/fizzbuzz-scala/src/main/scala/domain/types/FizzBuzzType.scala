package domain.types

import domain.model.FizzBuzzValue

abstract class FizzBuzzType {
  val TYPE_01 = 1
  val TYPE_02 = 2
  val TYPE_03 = 3
  
  def generate(number: Int): FizzBuzzValue
  
  protected def isFizz(number: Int): Boolean = number % 3 == 0
  protected def isBuzz(number: Int): Boolean = number % 5 == 0
  protected def isFizzBuzz(number: Int): Boolean = number % 15 == 0
}

object FizzBuzzType {
  def create(typeNum: Int): FizzBuzzType = typeNum match {
    case 1 => FizzBuzzType01
    case 2 => FizzBuzzType02  
    case 3 => FizzBuzzType03
    case _ => FizzBuzzTypeNotDefined
  }
}
