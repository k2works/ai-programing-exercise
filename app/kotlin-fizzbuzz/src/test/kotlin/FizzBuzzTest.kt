import application.AssertionFailedException
import application.FizzBuzzListCommand
import application.FizzBuzzValueCommand
import domain.model.FizzBuzzList
import domain.type.FizzBuzzType01
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows

class FizzBuzzTest {
    
    private val fizzBuzz = FizzBuzz()

    @Test
    fun `test_1を渡したら文字列1を返す`() {
        assertEquals("1", fizzBuzz.generate(1).value)
    }

    @Test
    fun `test_2を渡したら文字列2を返す`() {
        assertEquals("2", fizzBuzz.generate(2).value)
    }

    @Test
    fun `test_3を渡したら文字列Fizzを返す`() {
        assertEquals("Fizz", fizzBuzz.generate(3).value)
    }

    @Test
    fun `test_5を渡したら文字列Buzzを返す`() {
        assertEquals("Buzz", fizzBuzz.generate(5).value)
    }

    @Test
    fun `test_15を渡したら文字列FizzBuzzを返す`() {
        assertEquals("FizzBuzz", fizzBuzz.generate(15).value)
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
        assertEquals("1", fizzBuzzType1.generate(1).value)
    }

    // タイプ2の場合
    @Test
    fun `test_タイプ2_1を渡したら文字列1を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("1", fizzBuzzType2.generate(1).value)
    }

    @Test
    fun `test_タイプ2_3を渡したら文字列3を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("3", fizzBuzzType2.generate(3).value)
    }

    @Test
    fun `test_タイプ2_5を渡したら文字列5を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("5", fizzBuzzType2.generate(5).value)
    }

    @Test
    fun `test_タイプ2_15を渡したら文字列15を返す`() {
        val fizzBuzzType2 = FizzBuzz(FizzBuzz.createFizzBuzzType(2))
        assertEquals("15", fizzBuzzType2.generate(15).value)
    }

    // タイプ3の場合
    @Test
    fun `test_タイプ3_1を渡したら文字列1を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("1", fizzBuzzType3.generate(1).value)
    }

    @Test
    fun `test_タイプ3_3を渡したら文字列3を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("3", fizzBuzzType3.generate(3).value)
    }

    @Test
    fun `test_タイプ3_5を渡したら文字列5を返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("5", fizzBuzzType3.generate(5).value)
    }

    @Test
    fun `test_タイプ3_15を渡したら文字列FizzBuzzを返す`() {
        val fizzBuzzType3 = FizzBuzz(FizzBuzz.createFizzBuzzType(3))
        assertEquals("FizzBuzz", fizzBuzzType3.generate(15).value)
    }

    // カプセル化テスト
    @Test
    fun `test_配列を作成して取得できる`() {
        val fizzBuzz = FizzBuzz()
        fizzBuzz.generateList()
        val result = fizzBuzz.list
        assertEquals(100, result?.getValue()?.size)
        assertEquals("1", result?.getValue()?.get(0))
        assertEquals("FizzBuzz", result?.getValue()?.get(14))
    }

    // ファーストクラスコレクションテスト
    @Test
    fun `test_FizzBuzzListが正しく動作する`() {
        val list1 = FizzBuzzList(listOf("1", "2", "Fizz"))
        val list2 = FizzBuzzList(listOf("4", "Buzz"))
        val combined = list1.add(list2.getValue())
        
        assertEquals(3, list1.getValue().size)
        assertEquals(5, combined.getValue().size)
        assertEquals("Fizz", list1.getValue()[2])
        assertEquals("Buzz", combined.getValue()[4])
    }

    // Commandパターンテスト
    @Test
    fun `test_FizzBuzzValueCommandが正しく動作する`() {
        val type = FizzBuzzType01()
        val command = FizzBuzzValueCommand(type)
        val result = command.execute(15)
        
        assertEquals(15, result.number)
        assertEquals("FizzBuzz", result.value)
    }

    @Test
    fun `test_FizzBuzzListCommandが正しく動作する`() {
        val type = FizzBuzzType01()
        val command = FizzBuzzListCommand(type)
        val result = command.execute()
        
        assertEquals(100, result.getValue().size)
        assertEquals("1", result.getValue()[0])
        assertEquals("FizzBuzz", result.getValue()[14])
    }

    // 例外ケーステスト
    @Test
    fun `test_値は正の値のみ許可する`() {
        val type = FizzBuzzType01()
        val command = FizzBuzzValueCommand(type)
        
        val exception = assertThrows<AssertionFailedException> {
            command.execute(-1)
        }
        assertEquals("値は正の値のみ許可する", exception.message)
    }
}
