package application

import domain.types.FizzBuzzType
import domain.model.FizzBuzzList

class FizzBuzzListCommand(fizzBuzzType: FizzBuzzType) extends FizzBuzzCommand[Array[String]] {
  def execute(number: Int): Array[String] = {
    val fizzbuzzList = new FizzBuzzList(1, number, fizzBuzzType)
    fizzbuzzList.toArray
  }
}
