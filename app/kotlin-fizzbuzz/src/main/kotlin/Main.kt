fun main() {
    println("FizzBuzz Application")
    
    // 1から15までのテスト
    val result1to15 = FizzBuzz.printNumbers(1, 15)
    println("1から15まで:")
    result1to15.forEach { println(it) }
    
    println("\n1から100まで:")
    // 1から100までの完全なFizzBuzz
    val result1to100 = FizzBuzz.printNumbers(1, 100)
    result1to100.forEach { println(it) }
}
