package application

trait FizzBuzzCommand[T] {
  def execute(number: Int): T
}
