import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach

class FizzBuzzTest {
    private lateinit var fizzbuzz: FizzBuzz.Companion

    @BeforeEach
    fun setup() {
        fizzbuzz = FizzBuzz
    }

    @Test
    fun `test_1を渡したら文字列1を返す`() {
        assertEquals("1", fizzbuzz.generate(1))
    }

    @Test
    fun `test_2を渡したら文字列2を返す`() {
        assertEquals("2", fizzbuzz.generate(2))
    }

    @Test
    fun `test_3を渡したら文字列Fizzを返す`() {
        assertEquals("Fizz", fizzbuzz.generate(3))
    }

    @Test
    fun `test_5を渡したら文字列Buzzを返す`() {
        assertEquals("Buzz", fizzbuzz.generate(5))
    }

    @Test
    fun `test_15を渡したら文字列FizzBuzzを返す`() {
        assertEquals("FizzBuzz", fizzbuzz.generate(15))
    }
}
