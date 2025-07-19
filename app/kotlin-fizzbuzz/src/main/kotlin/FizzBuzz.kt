class FizzBuzz {
    companion object {
        // 定数定義（マジックナンバーの解消）
        private const val FIZZ_DIVISOR = 3
        private const val BUZZ_DIVISOR = 5
        private const val FIZZ_BUZZ_DIVISOR = 15
        const val DEFAULT_END_NUMBER = 100
    }

    private var _list: List<String>? = null
    val list: List<String>? get() = _list

    fun generate(number: Int, type: Int = 1): String {
        return when (type) {
            1 -> {
                if (number % FIZZ_BUZZ_DIVISOR == 0) {
                    "FizzBuzz"
                } else if (number % FIZZ_DIVISOR == 0) {
                    "Fizz"
                } else if (number % BUZZ_DIVISOR == 0) {
                    "Buzz"
                } else {
                    number.toString()
                }
            }
            2 -> number.toString()
            3 -> {
                if (number % FIZZ_BUZZ_DIVISOR == 0) {
                    "FizzBuzz"
                } else {
                    number.toString()
                }
            }
            else -> number.toString()
        }
    }

    fun generateList(): List<String> {
        _list = (1..DEFAULT_END_NUMBER).map { generate(it) }
        return _list!!
    }

    fun printNumbers(start: Int = 1, end: Int = DEFAULT_END_NUMBER): List<String> {
        return (start..end).map { generate(it) }
    }
}
