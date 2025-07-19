package domain.model

data class FizzBuzzValue(val number: Int, val value: String) {
    override fun toString(): String = value
}