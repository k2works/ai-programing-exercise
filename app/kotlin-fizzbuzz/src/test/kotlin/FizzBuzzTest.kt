import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FizzBuzzTest {

    @Test
    fun `test_1を渡したら文字列1を返す`() {
        assertEquals("1", FizzBuzz.generate(1))
    }

    @Test
    fun `test_2を渡したら文字列2を返す`() {
        assertEquals("2", FizzBuzz.generate(2))
    }

    @Test
    fun `test_3を渡したら文字列Fizzを返す`() {
        assertEquals("Fizz", FizzBuzz.generate(3))
    }

    @Test
    fun `test_5を渡したら文字列Buzzを返す`() {
        assertEquals("Buzz", FizzBuzz.generate(5))
    }

    @Test
    fun `test_15を渡したら文字列FizzBuzzを返す`() {
        assertEquals("FizzBuzz", FizzBuzz.generate(15))
    }

    @Test
    fun `test_1から3までの配列を返す`() {
        val result = FizzBuzz.printNumbers(1, 3)
        assertEquals(listOf("1", "2", "Fizz"), result)
    }

    @Test
    fun `test_1から15までの配列を返す`() {
        val result = FizzBuzz.printNumbers(1, 15)
        assertEquals(
            listOf(
                "1", "2", "Fizz", "4", "Buzz",
                "Fizz", "7", "8", "Fizz", "Buzz",
                "11", "Fizz", "13", "14", "FizzBuzz"
            ),
            result
        )
    }

    // タイプごとに出力を切り替えることができる
    // タイプ1の場合
    @Test
    fun `test_タイプ1_1を渡したら文字列1を返す`() {
        assertEquals("1", FizzBuzz.generate(1, 1))
    }

    // タイプ2の場合
    @Test
    fun `test_タイプ2_1を渡したら文字列1を返す`() {
        assertEquals("1", FizzBuzz.generate(1, 2))
    }

    @Test
    fun `test_タイプ2_3を渡したら文字列3を返す`() {
        assertEquals("3", FizzBuzz.generate(3, 2))
    }

    @Test
    fun `test_タイプ2_5を渡したら文字列5を返す`() {
        assertEquals("5", FizzBuzz.generate(5, 2))
    }

    @Test
    fun `test_タイプ2_15を渡したら文字列15を返す`() {
        assertEquals("15", FizzBuzz.generate(15, 2))
    }
}
