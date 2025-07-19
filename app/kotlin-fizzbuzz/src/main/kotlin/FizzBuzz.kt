class AssertionFailedException(message: String) : Exception(message)

object Assertions {
    fun require(condition: Boolean, message: String = "Assertion failed") {
        if (!condition) {
            throw AssertionFailedException(message)
        }
    }
}

data class FizzBuzzValue(val number: Int, val value: String) {
    override fun toString(): String = value
}

interface FizzBuzzCommand {
    fun execute(): Any
}

class FizzBuzzValueCommand(private val type: FizzBuzzType) : FizzBuzzCommand {
    override fun execute(): Any {
        throw UnsupportedOperationException("Use execute(number) instead")
    }
    
    fun execute(number: Int): FizzBuzzValue {
        Assertions.require(number > 0, "値は正の値のみ許可する")
        val value = type.generate(number)
        return FizzBuzzValue(number, value)
    }
}

class FizzBuzzListCommand(private val type: FizzBuzzType) : FizzBuzzCommand {
    override fun execute(): FizzBuzzList {
        val values = (1..FizzBuzz.DEFAULT_END_NUMBER).map { number ->
            FizzBuzzValueCommand(type).execute(number).value
        }
        return FizzBuzzList(values)
    }
}

class FizzBuzzList(private val value: List<String>) {
    fun getValue(): List<String> = value
    
    override fun toString(): String = value.toString()
    
    fun add(other: List<String>): FizzBuzzList {
        return FizzBuzzList(value + other)
    }
}

abstract class FizzBuzzType {
    abstract fun generate(number: Int): String
}

class FizzBuzzType01 : FizzBuzzType() {
    companion object {
        private const val FIZZ_DIVISOR = 3
        private const val BUZZ_DIVISOR = 5
        private const val FIZZ_BUZZ_DIVISOR = 15
    }

    override fun generate(number: Int): String {
        return if (number % FIZZ_BUZZ_DIVISOR == 0) {
            "FizzBuzz"
        } else if (number % FIZZ_DIVISOR == 0) {
            "Fizz"
        } else if (number % BUZZ_DIVISOR == 0) {
            "Buzz"
        } else {
            number.toString()
        }
    }
}

class FizzBuzzType02 : FizzBuzzType() {
    override fun generate(number: Int): String {
        return number.toString()
    }
}

class FizzBuzzType03 : FizzBuzzType() {
    companion object {
        private const val FIZZ_BUZZ_DIVISOR = 15
    }

    override fun generate(number: Int): String {
        return if (number % FIZZ_BUZZ_DIVISOR == 0) {
            "FizzBuzz"
        } else {
            number.toString()
        }
    }
}

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
