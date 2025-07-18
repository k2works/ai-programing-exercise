import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class HelloTest {
    @Test
    fun testGreeting() {
        assertEquals("hello world", greeting())
    }
}

fun greeting(): String {
    return "hello world"
}
