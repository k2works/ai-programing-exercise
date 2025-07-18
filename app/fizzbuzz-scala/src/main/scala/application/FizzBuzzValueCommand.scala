package application

import domain.types.FizzBuzzType

class FizzBuzzValueCommand(fizzBuzzType: FizzBuzzType) extends FizzBuzzCommand[String] {
  def execute(number: Int): String = 
    fizzBuzzType.generate(number).value
}
