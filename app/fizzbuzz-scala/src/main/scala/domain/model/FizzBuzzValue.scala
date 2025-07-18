package domain.model

case class FizzBuzzValue(number: Int, value: String) {
  require(number >= 0, "正の値のみ有効です")
  
  override def toString: String = s"$number:$value"
}
