import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.{FizzBuzzType, FizzBuzzType01, FizzBuzzType02, FizzBuzzType03}
import application.{FizzBuzzValueCommand, FizzBuzzListCommand}

class FizzBuzz(private val fizzBuzzType: FizzBuzzType) {
  
  def generate(number: Int): FizzBuzzValue = 
    fizzBuzzType.generate(number)

  def createList(start: Int, end: Int): FizzBuzzList =
    new FizzBuzzList(start, end, fizzBuzzType)
}

object FizzBuzz {
  def generate(number: Int, fizzBuzzType: Int = 1): String = {
    val typeStrategy = FizzBuzzType.create(fizzBuzzType)
    val command = new FizzBuzzValueCommand(typeStrategy)
    command.execute(number)
  }
  
  def createList(start: Int, end: Int): Array[String] = {
    val command = new FizzBuzzListCommand(FizzBuzzType01)
    command.execute(end)
  }
}
