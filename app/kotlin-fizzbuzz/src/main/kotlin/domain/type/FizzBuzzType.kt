package domain.type

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