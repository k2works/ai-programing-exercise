package domain.model

case class FizzBuzzList(values: List[FizzBuzzValue]) {
  val MaxCount = 100
  
  require(values.length <= MaxCount, s"上限は${MaxCount}件までです")
  
  def this(start: Int, end: Int, fizzBuzzType: domain.types.FizzBuzzType) = {
    this((start to end).map(fizzBuzzType.generate(_)).toList)
  }
  
  def add(value: FizzBuzzValue): FizzBuzzList = FizzBuzzList(values :+ value)
  
  def get(index: Int): Option[FizzBuzzValue] = values.lift(index)
  
  def toArray: Array[String] = values.map(_.value).toArray
  
  override def toString: String = values.map(_.toString).mkString(",")
}
