package domain.model

class FizzBuzzList(private val value: List<String>) {
    fun getValue(): List<String> = value
    
    override fun toString(): String = value.toString()
    
    fun add(other: List<String>): FizzBuzzList {
        return FizzBuzzList(value + other)
    }
}