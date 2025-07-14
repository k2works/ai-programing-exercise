import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

class FizzBuzzTest {
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
    
    @Test
    void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
    }
    
    @Test
    void test_5を渡したら文字列Buzzを返す() {
        assertEquals("Buzz", fizzbuzz.generate(5));
    }
}
