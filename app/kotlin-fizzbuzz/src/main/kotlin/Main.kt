private const val START_NUMBER = 1
private const val TEST_END_NUMBER = 15

fun main() {
    println("FizzBuzz Application")
    
    // Commandパターンを使用したFizzBuzz実行
    val type = FizzBuzz.createFizzBuzzType(1)
    val command = FizzBuzzListCommand(type)
    val result = command.execute()
    
    println("1から${FizzBuzz.DEFAULT_END_NUMBER}まで:")
    result.getValue().forEach { println(it) }
}
