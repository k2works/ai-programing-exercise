import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FizzBuzzTest {
    
    private val fizzBuzz = FizzBuzz()

    @Test
    fun `test_1を渡したら文字列1を返す`() {
        assertEquals("1", fizzBuzz.generate(1))
    }

    @Test
    fun `test_2を渡したら文字列2を返す`() {
        assertEquals("2", fizzBuzz.generate(2))
    }

    @Test
    fun `test_3を渡したら文字列Fizzを返す`() {
        assertEquals("Fizz", fizzBuzz.generate(3))
    }

    @Test
    fun `test_5を渡したら文字列Buzzを返す`() {
        assertEquals("Buzz", fizzBuzz.generate(5))
    }

    @Test
    fun `test_15を渡したら文字列FizzBuzzを返す`() {
        assertEquals("FizzBuzz", fizzBuzz.generate(15))
    }

    @Test
    fun `test_1から3までの配列を返す`() {
        val result = fizzBuzz.printNumbers(1, 3)
        assertEquals(listOf("1", "2", "Fizz"), result)
    }

    @Test
    fun `test_1から15までの配列を返す`() {
        val result = fizzBuzz.printNumbers(1, 15)
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
        val fizzBuzzType1 = FizzBuzz(FizzBuzz.createFizzBuzzType(1))
        assertEquals("1", fizzBuzzType1.generate(1))
    }

    // タイプ2の場合
    @Test
    fun `test_タイプ2_1を渡したら文字列1を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("1", fizzBuzzType2.generate(1))
    }

    @Test
    fun `test_タイプ2_3を渡したら文字列3を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("3", fizzBuzzType2.generate(3))
    }

    @Test
    fun `test_タイプ2_5を渡したら文字列5を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("5", fizzBuzzType2.generate(5))
    }

    @Test
    fun `test_タイプ2_15を渡したら文字列15を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("15", fizzBuzzType2.generate(15))
    }

    // タイプ3の場合
    @Test
    fun `test_タイプ3_1を渡したら文字列1を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("1", fizzBuzzType3.generate(1))
    }

    @Test
    fun `test_タイプ3_3を渡したら文字列3を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("3", fizzBuzzType3.generate(3))
    }

    @Test
    fun `test_タイプ3_5を渡したら文字列5を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("5", fizzBuzzType3.generate(5))
    }

    @Test
    fun `test_タイプ3_15を渡したら文字列FizzBuzzを返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("FizzBuzz", fizzBuzzType3.generate(15))
    }

    // カプセル化テスト
    @Test
    fun `test_配列を作成して取得できる`() {
        val fizzBuzz = FizzBuzz()
        fizzBuzz.generateList()
        val result = fizzBuzz.list
        assertEquals(100, result?.size)
        assertEquals("1", result?.get(0))
        assertEquals("FizzBuzz", result?.get(14))
    }
}
