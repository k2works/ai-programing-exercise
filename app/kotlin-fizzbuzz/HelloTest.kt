import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals


class HelloTest {
    @Test
    fun testGreeting() {
        assertEquals("hello world", greeting())
    }
}

class FizzBuzzTest {
    @Test
    fun testFizzBuzz_1() {
        assertEquals("1", fizzBuzz(1))
    }

    @Test
    fun testFizzBuzz_3() {
        assertEquals("Fizz", fizzBuzz(3))
    }

    @Test
    fun testFizzBuzz_5() {
        assertEquals("Buzz", fizzBuzz(5))
    }

    @Test
    fun testFizzBuzz_15() {
        assertEquals("FizzBuzz", fizzBuzz(15))
    }
}

fun fizzBuzz(n: Int): String {
    // 仮実装: TDDサイクルのため空実装
    return ""
}
