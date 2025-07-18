class FizzBuzz {
    companion object {
        fun generate(number: Int): String {
            return if (number % 15 == 0) {
                "FizzBuzz"
            } else if (number % 3 == 0) {
                "Fizz"
            } else if (number % 5 == 0) {
                "Buzz"
            } else {
                number.toString()
            }
        }

        fun printNumbers(start: Int = 1, end: Int = 100): List<String> {
            return (start..end).map { generate(it) }
        }
    }
}
