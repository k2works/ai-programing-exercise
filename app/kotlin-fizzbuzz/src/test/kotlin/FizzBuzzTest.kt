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
}
