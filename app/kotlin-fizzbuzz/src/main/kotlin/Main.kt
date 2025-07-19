private const val START_NUMBER = 1
private const val TEST_END_NUMBER = 15

fun main() {
    println("FizzBuzz Application")
    
    val fizzBuzz = FizzBuzz()

    // 1から15までのテスト
    val result1to15 = fizzBuzz.printNumbers(START_NUMBER, TEST_END_NUMBER)
    println("${START_NUMBER}から${TEST_END_NUMBER}まで:")
    result1to15.forEach { println(it) }

    println("\n${START_NUMBER}から${FizzBuzz.DEFAULT_END_NUMBER}まで:")
    // 1から100までの完全なFizzBuzz
    val result1to100 = fizzBuzz.printNumbers(START_NUMBER, FizzBuzz.DEFAULT_END_NUMBER)
    result1to100.forEach { println(it) }
}
