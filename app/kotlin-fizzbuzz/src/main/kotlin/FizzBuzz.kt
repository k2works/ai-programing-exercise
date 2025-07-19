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

    private var _list: List<String>? = null
    val list: List<String>? get() = _list

    fun generate(number: Int): String {
        return type.generate(number)
    }

    fun generateList(): List<String> {
        _list = (1..DEFAULT_END_NUMBER).map { generate(it) }
        return _list!!
    }

    fun printNumbers(start: Int = 1, end: Int = DEFAULT_END_NUMBER): List<String> {
        return (start..end).map { generate(it) }
    }
}
