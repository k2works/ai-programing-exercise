object FizzBuzz {
    // 定数定義（マジックナンバーの解消）
    private const val FIZZ_DIVISOR = 3
    private const val BUZZ_DIVISOR = 5
    private const val FIZZ_BUZZ_DIVISOR = 15
    const val DEFAULT_END_NUMBER = 100

    fun generate(number: Int, type: Int = 1): String {
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

    fun printNumbers(start: Int = 1, end: Int = DEFAULT_END_NUMBER): List<String> {
        return (start..end).map { generate(it) }
    }
}
