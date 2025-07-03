import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.CsvSource;
import static org.assertj.core.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.*;

@DisplayName("FizzBuzz テスト")
public class FizzBuzzTest {
    
    private FizzBuzz fizzBuzz;
    
    @BeforeEach
    void setUp() {
        fizzBuzz = new FizzBuzz();
    }
    
    @Test
    @DisplayName("数字の1は文字列の1を返す")
    void shouldReturn1WhenInput1() {
        assertThat(fizzBuzz.convert(1)).isEqualTo("1");
    }
    
    @Test
    @DisplayName("数字の2は文字列の2を返す")
    void shouldReturn2WhenInput2() {
        assertThat(fizzBuzz.convert(2)).isEqualTo("2");
    }
    
    @Test
    @DisplayName("3で割り切れる数字はFizzを返す")
    void shouldReturnFizzWhenInputIsDivisibleBy3() {
        assertThat(fizzBuzz.convert(3)).isEqualTo("Fizz");
        assertThat(fizzBuzz.convert(6)).isEqualTo("Fizz");
        assertThat(fizzBuzz.convert(9)).isEqualTo("Fizz");
    }
    
    @Test
    @DisplayName("5で割り切れる数字はBuzzを返す")
    void shouldReturnBuzzWhenInputIsDivisibleBy5() {
        assertThat(fizzBuzz.convert(5)).isEqualTo("Buzz");
        assertThat(fizzBuzz.convert(10)).isEqualTo("Buzz");
        assertThat(fizzBuzz.convert(20)).isEqualTo("Buzz");
    }
    
    @Test
    @DisplayName("15で割り切れる数字はFizzBuzzを返す")
    void shouldReturnFizzBuzzWhenInputIsDivisibleBy15() {
        assertThat(fizzBuzz.convert(15)).isEqualTo("FizzBuzz");
        assertThat(fizzBuzz.convert(30)).isEqualTo("FizzBuzz");
        assertThat(fizzBuzz.convert(45)).isEqualTo("FizzBuzz");
    }
    
    @ParameterizedTest
    @DisplayName("パラメータ化テスト：通常の数字")
    @ValueSource(ints = {1, 2, 4, 7, 8, 11, 13, 14, 16, 17, 19, 22, 23, 26, 28, 29})
    void shouldReturnNumberAsStringWhenInputIsNormalNumber(int number) {
        assertThat(fizzBuzz.convert(number)).isEqualTo(String.valueOf(number));
    }
    
    @ParameterizedTest
    @DisplayName("パラメータ化テスト：3の倍数")
    @ValueSource(ints = {3, 6, 9, 12, 18, 21, 24, 27})
    void shouldReturnFizzWhenInputIsMultipleOf3(int number) {
        assertThat(fizzBuzz.convert(number)).isEqualTo("Fizz");
    }
    
    @ParameterizedTest
    @DisplayName("パラメータ化テスト：5の倍数")
    @ValueSource(ints = {5, 10, 20, 25})
    void shouldReturnBuzzWhenInputIsMultipleOf5(int number) {
        assertThat(fizzBuzz.convert(number)).isEqualTo("Buzz");
    }
    
    @ParameterizedTest
    @DisplayName("パラメータ化テスト：15の倍数")
    @ValueSource(ints = {15, 30, 45, 60, 75, 90})
    void shouldReturnFizzBuzzWhenInputIsMultipleOf15(int number) {
        assertThat(fizzBuzz.convert(number)).isEqualTo("FizzBuzz");
    }
    
    @ParameterizedTest
    @DisplayName("CSVソースを使った総合テスト")
    @CsvSource({
        "1, 1",
        "2, 2",
        "3, Fizz",
        "4, 4",
        "5, Buzz",
        "6, Fizz",
        "7, 7",
        "8, 8",
        "9, Fizz",
        "10, Buzz",
        "11, 11",
        "12, Fizz",
        "13, 13",
        "14, 14",
        "15, FizzBuzz",
        "30, FizzBuzz",
        "100, Buzz"
    })
    void shouldReturnExpectedValueForGivenInput(int input, String expected) {
        assertThat(fizzBuzz.convert(input)).isEqualTo(expected);
    }
    
    @Test
    @DisplayName("負の数に対する例外テスト")
    void shouldThrowExceptionWhenInputIsNegative() {
        assertThatThrownBy(() -> fizzBuzz.convert(-1))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Input must be positive number");
    }
    
    @Test
    @DisplayName("0に対する例外テスト")
    void shouldThrowExceptionWhenInputIsZero() {
        assertThatThrownBy(() -> fizzBuzz.convert(0))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Input must be positive number");
    }
    
    @Test
    @DisplayName("学習テスト：Java Stream API の理解")
    void learningTestForJavaStreamApi() {
        // Java 8 Stream API を使った FizzBuzz の実装パターンを学習
        java.util.stream.IntStream.rangeClosed(1, 15)
                .mapToObj(i -> {
                    if (i % 15 == 0) return "FizzBuzz";
                    if (i % 3 == 0) return "Fizz";
                    if (i % 5 == 0) return "Buzz";
                    return String.valueOf(i);
                })
                .forEach(System.out::println);
        
        // このテストは実際の実装確認ではなく、Streamの動作確認
        assertThat(java.util.stream.IntStream.rangeClosed(1, 15).count()).isEqualTo(15);
    }
    
    @Test
    @DisplayName("学習テスト：文字列の等価性")
    void learningTestForStringEquality() {
        String fizz1 = "Fizz";
        String fizz2 = "Fizz";
        String fizz3 = new String("Fizz");
        
        // 文字列プールによる参照の同一性
        assertThat(fizz1).isSameAs(fizz2);
        assertThat(fizz1).isNotSameAs(fizz3);
        
        // 文字列の値の等価性
        assertThat(fizz1).isEqualTo(fizz2);
        assertThat(fizz1).isEqualTo(fizz3);
    }
    
    @Test
    @DisplayName("学習テスト：例外の詳細")
    void learningTestForExceptionDetails() {
        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> fizzBuzz.convert(-5)
        );
        
        assertThat(exception.getMessage()).isEqualTo("Input must be positive number");
        assertThat(exception.getClass()).isEqualTo(IllegalArgumentException.class);
    }
}
