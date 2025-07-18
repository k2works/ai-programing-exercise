class FizzBuzz {
    companion object {
        fun generate(number: Int): String {
            return if (number % 3 == 0) {
                "Fizz"
            } else {
                number.toString()
            }
        }
    }
}
