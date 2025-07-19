import application.FizzBuzzListCommand
import application.FizzBuzzValueCommand
import domain.model.FizzBuzzList
import domain.model.FizzBuzzValue
import domain.type.FizzBuzzType
import domain.type.FizzBuzzType01
import domain.type.FizzBuzzType02
import domain.type.FizzBuzzType03

class FizzBuzz(private val type: FizzBuzzType = FizzBuzzType01()) {
    companion object {
        const val DEFAULT_END_NUMBER = 100

        fun createFizzBuzzType(type: Int): FizzBuzzType {
            return when (type) {
                1 -> FizzBuzzType01()
                2 -> FizzBuzzType02()
                3 -> FizzBuzzType03()
                else -> FizzBuzzType01()
            }
        }
    }

    private var _list: FizzBuzzList? = null
    val list: FizzBuzzList? get() = _list

    fun generate(number: Int): FizzBuzzValue {
        val command = FizzBuzzValueCommand(type)
        return command.execute(number)
    }

    fun generateList(): FizzBuzzList {
        val command = FizzBuzzListCommand(type)
        _list = command.execute()
        return _list!!
    }

    fun printNumbers(start: Int = 1, end: Int = DEFAULT_END_NUMBER): List<String> {
        return (start..end).map { number ->
            generate(number).value
        }
    }
}