import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.util.List;

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
    
    @Test
    void test_15を渡したら文字列FizzBuzzを返す() {
        assertEquals("FizzBuzz", fizzbuzz.generate(15));
    }
    
    @Test
    void test_1から100までのFizzBuzzを生成する() {
        List<String> result = fizzbuzz.generateList(100);
        
        assertEquals(100, result.size());
        assertEquals("1", result.get(0));
        assertEquals("2", result.get(1));
        assertEquals("Fizz", result.get(2));
        assertEquals("4", result.get(3));
        assertEquals("Buzz", result.get(4));
        assertEquals("Fizz", result.get(5));
        assertEquals("FizzBuzz", result.get(14));
        assertEquals("FizzBuzz", result.get(29));
        assertEquals("FizzBuzz", result.get(59));
        assertEquals("FizzBuzz", result.get(89));
        assertEquals("Buzz", result.get(99));
    }
}
