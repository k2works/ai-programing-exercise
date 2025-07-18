import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class FizzBuzzTest {
    @Test
    fun `test_1を渡したら文字列1を返す`() {
        assertEquals("1", FizzBuzz.generate(1))
    }

    @Test
    fun `test_2を渡したら文字列2を返す`() {
        assertEquals("2", FizzBuzz.generate(2))
    }
}
